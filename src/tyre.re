/*
 * Copyright (c) 2016 Gabriel Radanne <drupyog@zoho.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

module Seq = {
  include Seq;

  let of_list = l => {
    let rec aux = (l, ()) =>
      switch (l) {
      | [] => Seq.Nil
      | [x, ...tail] => [@implicit_arity] Seq.Cons(x, aux(tail))
      };

    aux(l);
  };
  let to_rev_list = gen => fold_left((acc, x) => [x, ...acc], [], gen);
  let to_list = gen => List.rev(to_rev_list(gen));
};

let map_3 = (f, (x, y, z)) => (x, y, f(z));

/** {2 The various types} */;

module T = {
  type conv('a, 'b) = {
    to_: 'a => 'b,
    from_: 'b => 'a,
  };

  type raw('a) =
    /* We store a compiled regex to efficiently check string when unparsing. */
    | Regexp(Re.t, Lazy.t(Re.re)): raw(string)
    | Conv(raw('a), conv('a, 'b)): raw('b)
    | Opt(raw('a)): raw(option('a))
    | Alt(raw('a), raw('b)): raw([ | `Left('a) | `Right('b)])
    | Seq(raw('a), raw('b)): raw(('a, 'b))
    | Prefix(raw('b), raw('a)): raw('a)
    | Suffix(raw('a), raw('b)): raw('a)
    | Rep(raw('a)): raw(Seq.t('a))
    | Mod(Re.t => Re.t, raw('a)): raw('a);

  type wit(_) =
    | Lit(int): wit(string)
    | Conv(wit('a), conv('a, 'b)): wit('b)
    | Opt(Re.Mark.t, wit('a)): wit(option('a))
    | Alt(Re.Mark.t, wit('a), wit('b)): wit([ | `Left('a) | `Right('b)])
    | Seq(wit('a), wit('b)): wit(('a, 'b))
    | Rep(int, wit('a), Re.re): wit(Seq.t('a));
};

type t('a) = T.raw('a);

let regex = (x): t(_) => {
  let re = lazy(Re.(compile @@ whole_string @@ no_group(x)));
  [@implicit_arity] Regexp(x, re);
};

let pcre = s => regex @@ Re.Pcre.re(s);

/* Converters

      The exception matching of converters is handled by {!Tyre.exec} directly.
   */
let conv = (to_, from_, x): t(_) =>
  [@implicit_arity] Conv(x, {to_, from_});

let seq = (a, b): t(_) => [@implicit_arity] Seq(a, b);
let alt = (a, b): t(_) => [@implicit_arity] Alt(a, b);

let prefix = (x, a): t(_) => [@implicit_arity] Prefix(x, a);
let suffix = (a, x): t(_) => [@implicit_arity] Suffix(a, x);
let opt = (a): t(_) => Opt(a);

module Infix = {
  let (<|>) = alt;
  let (<&>) = seq;

  let ( *> ) = prefix;
  let ( <* ) = suffix;
};
include Infix;

let rep = (x): t(_) => Rep(x);
let rep1 = x => x <&> rep(x);

/* [modifier] is unsafe in general (for example [modifier Re.group]).
      It shouldn't be exposed to the user.
   */
let modifier = (f, re): t(_) => [@implicit_arity] Mod(f, re);

let word = re => modifier(Re.word, re);
let whole_string = re => modifier(Re.whole_string, re);
let longest = re => modifier(Re.longest, re);
let shortest = re => modifier(Re.shortest, re);
let first = re => modifier(Re.first, re);
let greedy = re => modifier(Re.greedy, re);
let non_greedy = re => modifier(Re.non_greedy, re);
let nest = re => modifier(Re.nest, re);

module Regex = {
  open! Re;

  /** [0-9]+ */

  let pos_int = rep1(digit);

  /** -?[0-9]+ */

  let int = seq([opt(char('-')), pos_int]);

  /** -?[0-9]+( .[0-9]* )? */

  let float =
    seq([
      opt(char('-')),
      rep1(digit),
      opt(seq([char('.'), rep(digit)])),
    ]);

  /** true|false */

  let bool = alt([str("true"), str("false")]);
};

let unit = (s, re) => conv(_ => (), () => s, regex(re));

let start = unit("", Re.start);
let stop = unit("", Re.stop);

let str = s => unit(s, Re.str(s));

let char = c => {
  let s = String.make(1, c);
  unit(s, Re.char(c));
};

let blanks = unit("", Re.rep(Re.blank));

let pos_int = conv(int_of_string, string_of_int, regex(Regex.pos_int));

let int = conv(int_of_string, string_of_int, regex(Regex.int));

let float = conv(float_of_string, string_of_float, regex(Regex.float));

let bool = conv(bool_of_string, string_of_bool, regex(Regex.bool));

let list = e => conv(Seq.to_list, Seq.of_list, rep(e));

let terminated_list = (~sep, e) => list(e <* sep);
let separated_list = (~sep, e) => {
  let e = opt(e <&> list(sep *> e));
  let to_ =
    fun
    | None => []
    | Some((h, t)) => [h, ...t]
  and from_ =
    fun
    | [] => None
    | [h, ...t] => Some((h, t));

  conv(to_, from_, e);
};

/** {2 Witness} */;

/** A witness is a string such that [exec (compile re) (witness re) = true].
    The computation of the witness is deterministic and should result in
    a small example.

    It is used in [eval] for the part of the regex that are ignored.
*/;

let rec witnesspp: type a. (Format.formatter, t(a)) => unit =
  (ppf, tre) =>
    T.(
      switch (tre) {
      | [@implicit_arity] Regexp(re, _) =>
        Format.pp_print_string(ppf) @@ Re.witness(re)
      | [@implicit_arity] Conv(tre, _) => witnesspp(ppf, tre)
      | Opt(_) => ()
      | [@implicit_arity] Alt(tre1, _) => witnesspp(ppf, tre1)
      | [@implicit_arity] Seq(tre1, tre2) =>
        witnesspp(ppf, tre1);
        witnesspp(ppf, tre2);
      | [@implicit_arity] Prefix(tre1, tre2) =>
        witnesspp(ppf, tre1);
        witnesspp(ppf, tre2);
      | [@implicit_arity] Suffix(tre1, tre2) =>
        witnesspp(ppf, tre1);
        witnesspp(ppf, tre2);
      | Rep(_) => ()
      | [@implicit_arity] Mod(_, tre) => witnesspp(ppf, tre)
      }
    );

/** {2 Evaluation functions} */;

/** Evaluation is the act of filling the holes. */;

let pstr = Format.pp_print_string;
let rec pprep = (f, ppf, seq) =>
  switch (seq()) {
  | Seq.Nil => ()
  | [@implicit_arity] Cons(x, seq) =>
    f(ppf, x);
    pprep(f, ppf, seq);
  };

let rec evalpp: type a. (t(a), Format.formatter, a) => unit =
  (tre, ppf) =>
    T.(
      switch (tre) {
      | [@implicit_arity] Regexp(_, lazy cre) => (
          fun
          | v => {
              if ((!) @@ Re.execp(cre, v)) {
                invalid_arg @@
                Printf.sprintf(
                  "Tyre.eval: regexp not respected by \"%s\".",
                  v,
                );
              };
              pstr(ppf, v);
            }
        )
      | [@implicit_arity] Conv(tre, conv) => (
          v => evalpp(tre, ppf, conv.from_(v))
        )
      | Opt(p) => (
          fun
          | None => pstr(ppf, "")
          | Some(x) => evalpp(p, ppf, x)
        )
      | [@implicit_arity] Seq(tre1, tre2) => (
          ((x1, x2)) => {
            evalpp(tre1, ppf, x1);
            evalpp(tre2, ppf, x2);
          }
        )
      | [@implicit_arity] Prefix(tre_l, tre) => (
          v => {
            witnesspp(ppf, tre_l);
            evalpp(tre, ppf, v);
          }
        )
      | [@implicit_arity] Suffix(tre, tre_g) => (
          v => {
            evalpp(tre, ppf, v);
            witnesspp(ppf, tre_g);
          }
        )
      | [@implicit_arity] Alt(treL, treR) => (
          fun
          | `Left(x) => evalpp(treL, ppf, x)
          | `Right(x) => evalpp(treR, ppf, x)
        )
      | Rep(tre) => pprep(evalpp(tre), ppf)
      | [@implicit_arity] Mod(_, tre) => evalpp(tre, ppf)
      }
    );

let eval = tre => Format.asprintf("%a", evalpp(tre));

/** {2 matching} */;

/** {3 Regexp construction}

    In order to record how we constructed the regexp and how to later
    extract information, we build a witness containing all the tools we need.

    Each alternative is marked with {!Re.mark}. We store the markid in order
    to be able to guess the branch matched.
*/;

let rec build: type a. (int, t(a)) => (int, T.wit(a), Re.t) = {
  open! Re;
  T.(
    i =>
      fun
      | [@implicit_arity] Regexp(re, _) => (
          i + 1,
          Lit(i),
          group @@ no_group(re),
        )
      | [@implicit_arity] Conv(e, conv) => {
          let (i', w, re) = build(i, e);
          (i', [@implicit_arity] Conv(w, conv), re);
        }
      | Opt(e) => {
          let (i', w, (id, re)) = map_3(mark) @@ build(i, e);
          (i', [@implicit_arity] Opt(id, w), opt(re));
        }
      | [@implicit_arity] Alt(e1, e2) => {
          let (i', w1, (id1, re1)) = map_3(mark) @@ build(i, e1);
          let (i'', w2, re2) = build(i', e2);
          (i'', [@implicit_arity] Alt(id1, w1, w2), alt([re1, re2]));
        }
      | [@implicit_arity] Prefix(e_ign, e) => {
          let (i', w, re) = build(i, e);
          let (_, _, re_ign) = build(1, e_ign);
          (i', w, seq([no_group(re_ign), re]));
        }
      | [@implicit_arity] Suffix(e, e_ign) => {
          let (i', w, re) = build(i, e);
          let (_, _, re_ign) = build(1, e_ign);
          (i', w, seq([re, no_group(re_ign)]));
        }
      | [@implicit_arity] Seq(e1, e2) => {
          let (i', w1, re1) = build(i, e1);
          let (i'', w2, re2) = build(i', e2);
          (i'', [@implicit_arity] Seq(w1, w2), seq([re1, re2]));
        }
      | Rep(e) => {
          let (_, w, re) = build(1, e);
          (
            i + 1,
            [@implicit_arity] Rep(i, w, Re.compile(re)),
            group @@ rep @@ no_group(re),
          );
        }
      | [@implicit_arity] Mod(f, e) => {
          let (i', w, re) = build(i, e);
          (i', w, f(re));
        }
  );
};

/** {3 Extraction.} */;

/** Extracting is just a matter of following the witness.
    We just need to take care of counting where we are in the matching groups.

    To avoid copy, we pass around the original string (and we use positions).
*/

[@specialize]
let rec extract: type a. (~original: string, T.wit(a), Re.Group.t) => a =
  (~original, rea, s) =>
    T.(
      switch (rea) {
      | Lit(i) => Re.Group.get(s, i)
      | [@implicit_arity] Conv(w, conv) =>
        let v = extract(~original, w, s);
        conv.to_(v);
      | [@implicit_arity] Opt(id, w) =>
        if ((!) @@ Re.Mark.test(s, id)) {
          None;
        } else {
          Some(extract(~original, w, s));
        }
      | [@implicit_arity] Alt(i1, w1, w2) =>
        if (Re.Mark.test(s, i1)) {
          `Left(extract(~original, w1, s));
        } else {
          /* Invariant: Alt produces [Re.alt [e1 ; e2]] */
          `Right(extract(~original, w2, s));
        }
      | [@implicit_arity] Seq(e1, e2) =>
        let v1 = extract(~original, e1, s);
        let v2 = extract(~original, e2, s);
        (v1, v2);
      | [@implicit_arity] Rep(i, e, re) =>
        extract_list(~original, e, re, i, s)
      }
    )

/** We need to re-match the string for lists, in order to extract
    all the elements.
    Re doesn't offer the possibility to keep the results when
    grouping under a star (one could argue it's theoretically not
    possible as it would be equivalent to counting in an automaton).
*/

[@specialize]
and extract_list:
  type a. (~original: string, T.wit(a), Re.re, int, Re.Group.t) => Seq.t(a) =
  (~original, e, re, i, s) => {
    let aux = extract(~original, e);
    let (pos, pos') = Re.Group.offset(s, i);
    let len = pos' - pos;
    Seq.map(aux) @@ Re.Seq.all(~pos, ~len, re, original);
  };

/** {4 Multiple match} */;

type route(+'r) =
  | Route(t('a), 'a => 'r): route('r);

let route = (re, f) => [@implicit_arity] Route(re, f);

let (-->) = route;

type wit_route('r) =
  | WRoute(Re.Mark.t, T.wit('a), 'a => 'r): wit_route('r);

/* It's important to keep the order here, since Re will choose
      the first regexp if there is ambiguity.
   */
let rec build_route_aux = (i, rel, wl) =>
  fun
  | [] => (List.rev(rel), List.rev(wl))
  | [[@implicit_arity] Route(tre, f), ...l] => {
      let (i', wit, re) = build(i, tre);
      let (id, re) = Re.mark(re);
      let w = [@implicit_arity] WRoute(id, wit, f);
      build_route_aux(i', [re, ...rel], [w, ...wl], l);
    };

let build_route = l => build_route_aux(1, [], [], l);

let rec extract_route = (~original, wl, subs) =>
  switch (wl) {
  | [] =>
    /* Invariant: At least one of the regexp of the alternative matches. */
    assert(false)
  | [[@implicit_arity] WRoute(id, wit, f), ...wl] =>
    if (Re.Mark.test(subs, id)) {
      f(extract(~original, wit, subs));
    } else {
      extract_route(~original, wl, subs);
    }
  };

/** {4 Compilation and execution} */;

type info('r) =
  | One(T.wit('r))
  | Routes(list(wit_route('r)));

type re('a) = {
  info: info('a),
  cre: Re.re,
};

let compile = tre => {
  let (_, wit, re) = build(1, tre);
  let cre = Re.compile(re);
  {info: One(wit), cre};
};

let route = l => {
  let (rel, wl) = build_route(l);
  let cre = Re.compile @@ Re.alt(rel);
  {info: Routes(wl), cre};
};

type error('a) = [ | `NoMatch(re('a), string) | `ConverterFailure(exn)];

let extract_with_info = (~info, ~original, subs) =>
  switch (info) {
  | One(w) => extract(~original, w, subs)
  | Routes(wl) => extract_route(~original, wl, subs)
  };

[@inline]
let exec = (~pos=?, ~len=?, {info, cre} as tcre, original) =>
  switch (Re.exec_opt(~pos?, ~len?, cre, original)) {
  | None => Result.Error(`NoMatch((tcre, original)))
  | Some(subs) =>
    try(Result.Ok(extract_with_info(~info, ~original, subs))) {
    | exn => Result.Error(`ConverterFailure(exn))
    }
  };

let execp = (~pos=?, ~len=?, {cre, _}, original) =>
  Re.execp(~pos?, ~len?, cre, original);

let all_seq = (~pos=?, ~len=?, {info, cre}, original) => {
  let seq = Re.Seq.all(~pos?, ~len?, cre, original);
  let get_res = subs => extract_with_info(~info, ~original, subs);
  Seq.map(get_res, seq);
};

let all = (~pos=?, ~len=?, tcre, original) =>
  try(Result.Ok(Seq.to_list @@ all_seq(~pos?, ~len?, tcre, original))) {
  | exn => Result.Error(`ConverterFailure(exn))
  };

/** Pretty printers */;

let sexp = (ppf, s, fmt) =>
  Format.fprintf(ppf, "@[<3>(%s@ " ^^ fmt ^^ ")@]", s);

/* Only in the stdlib since 4.02, so we copy. */
let rec pp_list = (pp, ppf) =>
  fun
  | [] => ()
  | [v] => pp(ppf, v)
  | [v, ...vs] => {
      pp(ppf, v);
      Format.pp_print_space(ppf, ());
      pp_list(pp, ppf, vs);
    };

let rec pp: type a. (_, t(a)) => unit =
  ppf =>
    T.(
      fun
      | [@implicit_arity] Regexp(re, _) => sexp(ppf, "Re", "%a", Re.pp, re)
      | [@implicit_arity] Conv(tre, _) => sexp(ppf, "Conv", "%a", pp, tre)
      | Opt(tre) => sexp(ppf, "Opt", "%a", pp, tre)
      | [@implicit_arity] Alt(tre1, tre2) =>
        sexp(ppf, "Alt", "%a@ %a", pp, tre1, pp, tre2)
      | [@implicit_arity] Seq(tre1, tre2) =>
        sexp(ppf, "Seq", "%a@ %a", pp, tre1, pp, tre2)
      | [@implicit_arity] Prefix(tre1, tre2) =>
        sexp(ppf, "Prefix", "%a@ %a", pp, tre1, pp, tre2)
      | [@implicit_arity] Suffix(tre1, tre2) =>
        sexp(ppf, "Suffix", "%a@ %a", pp, tre1, pp, tre2)
      | Rep(tre) => sexp(ppf, "Rep", "%a", pp, tre)
      | [@implicit_arity] Mod(_, tre) => sexp(ppf, "Mod", "%a", pp, tre)
    );

let rec pp_wit: type a. (_, T.wit(a)) => unit =
  ppf =>
    T.(
      fun
      | Lit(i) => sexp(ppf, "Lit", "%i", i)
      | [@implicit_arity] Conv(tre, _) =>
        sexp(ppf, "Conv", "%a", pp_wit, tre)
      | [@implicit_arity] Opt(_, tre) => sexp(ppf, "Opt", "%a", pp_wit, tre)
      | [@implicit_arity] Alt(_, tre1, tre2) =>
        sexp(ppf, "Alt", "%a@ %a", pp_wit, tre1, pp_wit, tre2)
      | [@implicit_arity] Seq(tre1, tre2) =>
        sexp(ppf, "Seq", "%a@ %a", pp_wit, tre1, pp_wit, tre2)
      | [@implicit_arity] Rep(i, w, re) =>
        sexp(ppf, "Rep", "%i@ %a@ %a", i, pp_wit, w, Re.pp_re, re)
    );

let pp_wit_route: type a. (_, wit_route(a)) => unit =
  (ppf, [@implicit_arity] WRoute(_, w, _)) => pp_wit(ppf, w);

let pp_re = ppf =>
  fun
  | {info: One(w), cre} =>
    sexp(ppf, "One", "%a@ %a", Re.pp_re, cre, pp_wit, w)
  | {info: Routes(wl), cre} =>
    sexp(ppf, "Route", "%a@ %a", Re.pp_re, cre, pp_list(pp_wit_route), wl);

let pp_error = (ppf): (error(_) => unit) =>
  fun
  | `NoMatch(re, s) => Format.fprintf(ppf, "`NoMatch (%a, %s)", pp_re, re, s)
  | `ConverterFailure(exn) =>
    Format.pp_print_string(ppf) @@ Printexc.to_string(exn);

module Internal = {
  include T;

  let to_t = x => x;
  let from_t = x => x;

  let build = build;
  let extract = extract;
};
