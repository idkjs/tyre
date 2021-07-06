/** {1 Typed regular expressions} */;

/**
Tyre is a set of combinators to build type-safe regular expressions, allowing automatic extraction and modification of matched groups.

Tyre is bi-directional: a typed regular expressions can be used both for {{!matching}matching} and {{!eval}evaluation}. Multiple tyregexs can be combined in order to do {{!routing}routing} in similar manner as switches/pattern matching.
Typed regular expressions are strictly as expressive as regular expressions from {{:https://github.com/ocaml/ocaml-re}re} (and are, as such, {b regular} expressions, not PCREs). Performances should be exactly the same.

{[
# let dim = Tyre.( str"dim:" *> int <&> str"x" *> int ) ;;
val dim : (int * int) Tyre.t

# let dim_re = Tyre.compile dim ;;
val dim_re : (int * int) Tyre.re

# Tyre.exec dim_re "dim:3x4" ;;
- : (int * int, (int * int) Tyre.error) result = Result.Ok (3, 4)

# Tyre.eval dim (2, 5) ;;
- : string = "dim:2x5"
]}

{{:https://github.com/paurkedal/ppx_regexp#ppx_tyre---syntax-support-for-tyre-routes}ppx_tyre} allows to use the usual regular syntax, if prefered:

{[
# let dim = [%tyre "dim:(?&int)x(?&int)"] ;;
val dim : (int * int) Tyre.t
]}

*/;

/** A typed regular expression.

    The type variable is the type of the returned value when the typed regular expression (tyregex) is executed.

    For example [tyre : int t] can be used to return an [int]. In the rest of the documentation, we will use «[tyre]» to designate a value of type {!t}.
*/

type t('a);

/** {1 Combinators} */;

/** [pcre s] is a tyregex that matches the PCRE [s] and return the
    corresponding string.
    Groups in [s] are ignored.
*/

let pcre: string => t(string);

/** [regex re] is a tyregex that matches [re] and return the corresponding string.
    Groups inside [re] are erased.
*/

let regex: Re.t => t(string);

/** [conv to_ from_ tyre] matches the same text as [tyre], but converts back and forth to a different data type.

    [to_] is allowed to raise an exception [exn].
    In this case, {!exec} will return [`ConverterFailure exn].

For example, this is the implementation of {!pos_int}:

{[
let pos_int =
  Tyre.conv
    int_of_string string_of_int
    (Tyre.regex (Re.rep1 Re.digit))
]}
*/

let conv: ('a => 'b, 'b => 'a, t('a)) => t('b);

/** [opt tyre] matches either [tyre] or the empty string. Similar to {!Re.opt}. */

let opt: t('a) => t(option('a));

/** [alt tyreL tyreR] matches either [tyreL] (and will then return [`Left v]) or [tyreR] (and will then return [`Right v]).
*/

let alt: (t('a), t('b)) => t([ | `Left('a) | `Right('b)]);

/** {2 Repetitions} */;

/** [rep tyre] matches [tyre] zero or more times. Similar to {!Re.rep}.

    For {{!matching}matching}, [rep tyre] will matches the string a first time, then [tyre] will be used to walk the matched part to extract values.
*/

let rep: t('a) => t(Seq.t('a));

/** [rep1 tyre] is [seq tyre (rep tyre)]. Similar to {!Re.rep1}. */

let rep1: t('a) => t(('a, Seq.t('a)));

/** {2 Sequences} */;

/** [seq tyre1 tyre2] matches [tyre1] then [tyre2] and return both values. */

let seq: (t('a), t('b)) => t(('a, 'b));

/** [prefix tyre_i tyre] matches [tyre_i], ignores the result, and then matches [tyre] and returns its result. Converters in [tyre_i] are never called.
*/

let prefix: (t(_), t('a)) => t('a);

/** Same as [prefix], but reversed. */

let suffix: (t('a), t(_)) => t('a);

/** {2 Infix operators} */;

/** [t <|> t'] is [alt t t']. */

let (<|>): (t('a), t('b)) => t([ | `Left('a) | `Right('b)]);

/** [t <&> t'] is [seq t t']. */

let (<&>): (t('a), t('b)) => t(('a, 'b));

/** [ ti *> t ] is [prefix ti t]. */

let ( *> ): (t(_), t('a)) => t('a);

/** [ t <* ti ] is [suffix t ti]. */

let ( <* ): (t('a), t(_)) => t('a);

module Infix: {
  /** [t <|> t'] is [alt t t']. */

  let (<|>): (t('a), t('b)) => t([ | `Left('a) | `Right('b)]);

  /** [t <&> t'] is [seq t t']. */

  let (<&>): (t('a), t('b)) => t(('a, 'b));

  /** [ ti *> t ] is [prefix ti t]. */

  let ( *> ): (t(_), t('a)) => t('a);

  /** [ t <* ti ] is [suffix t ti]. */

  let ( <* ): (t('a), t(_)) => t('a);
};

/** {2 Useful combinators} */;

/** [str s] matches [s] and evaluates to [s]. */

let str: string => t(unit);

/** [char c] matches [c] and evaluates to [c]. */

let char: char => t(unit);

/** [blanks] matches [Re.(rep blank)] and doesn't return anything. */

let blanks: t(unit);

/** [int] matches [-?[0-9]+] and returns the matched integer.

    Integers that do not fit in an [int] will fail.
*/

let int: t(int);

/** [pos_int] matches [[0-9]+] and returns the matched positive integer.

    Integers that do not fit in an [int] will fail.
*/

let pos_int: t(int);

/** [float] matches [-?[0-9]+( .[0-9]* )?] and returns the matched floating point number.

    Floating point numbers that do not fit in a [float] returns {!infinity} or {!neg_infinity}.
*/

let float: t(float);

/** [bool] matches [true|false] and returns the matched boolean. */

let bool: t(bool);

/** [list e] is similar to [rep e], but returns a list. */

let list: t('a) => t(list('a));

/** [terminated_list ~sep tyre] is [ list (tyre <* sep) ]. */

let terminated_list: (~sep: t(_), t('a)) => t(list('a));

/** [separated_list ~sep tyre] is equivalent to [opt (e <&> list (sep *> e))]. */

let separated_list: (~sep: t(_), t('a)) => t(list('a));

/** {2 Other combinators}

    See {!Re} for details on the semantics of those combinators. */;

let start: t(unit);
let stop: t(unit);

let word: t('a) => t('a);
let whole_string: t('a) => t('a);
let longest: t('a) => t('a);
let shortest: t('a) => t('a);
let first: t('a) => t('a);
let greedy: t('a) => t('a);
let non_greedy: t('a) => t('a);
let nest: t('a) => t('a);

/** {1:matching Matching} */;

/** A compiled typed regular expression. */

type re('a);

/** [compile tyre] is the compiled tyregex representing [tyre].
*/

let compile: t('a) => re('a);

type error('a) = [ | `NoMatch(re('a), string) | `ConverterFailure(exn)];

let pp_error: (Format.formatter, error(_)) => unit;

/** [exec ctyre s] matches the string [s] using
    the compiled tyregex [ctyre] and returns the extracted value.

    Returns [Error (`NoMatch (tyre, s)] if [tyre] doesn't match [s].
    Returns [Error (`ConverterFailure exn)] if a converter failed with the exception [exn].

    @param pos Optional beginning of the string (default 0)
    @param len Length of the substring of [str] that can be matched (default to the end of the string)
*/

let exec:
  (~pos: int=?, ~len: int=?, re('a), string) => Result.result('a, error('a));

/** [execp ctyre s] returns [true] if [ctyre] matches [s]. Converters
    are never called.

    @param pos Optional beginning of the string (default 0)
    @param len Length of the substring of [str] that can be matched (default to the end of the string)

    @since 0.1.1
*/

let execp: (~pos: int=?, ~len: int=?, re('a), string) => bool;

/** {2:repeat Repeated Matching} */;

/** [all ctyre s] calls to {!exec} repeatedly and returns the list of all the matches. */

let all:
  (~pos: int=?, ~len: int=?, re('a), string) =>
  Result.result(list('a), error('a));

/** [all_seq ctyre s] is [all ctyre s] but returns a {!gen} instead. Matches
    are enumerated lazily.

    Exceptions raised by converters are not caught.
*/

let all_seq: (~pos: int=?, ~len: int=?, re('a), string) => Seq.t('a);

/** {2:routing Routing} */;

type route(+'a) =
  | /** A route is a pair of a tyregex and a handler.
    When the tyregex is matched, the function is called with the
    result of the matching.
*/
    Route(
      t('x),
      'x => 'a,
    )
    : route('a);

/** [tyre --> f] is [Route (tyre, f)]. */

let (-->): (t('x), 'x => 'a) => route('a);

/** [route [ tyre1 --> f1 ; tyre2 --> f2 ]] produces a compiled
    tyregex such that, if [tyre1] matches, [f1] is called, and so on.

    The compiled tyregex shoud be used with {!exec}.
*/

let route: list(route('a)) => re('a);

/** {1:eval Evaluating} */;

/** [eval tyre v] returns a string [s] such that [exec (compile tyre) s = v].

    Note that such string [s] is not unique. [eval] will usually returns a very simple witness. */

let eval: (t('a), 'a) => string;

/** [evalpp tyre ppf v] is equivalent to [Format.fprintf ppf "%s" (eval tyre v)], but more efficient.

    Is is generally used with ["%a"]:
{[
let my_pp = Tyre.evalpp tyre in
Format.printf "%a@." my_pp v
]}
*/

let evalpp: (t('a), Format.formatter, 'a) => unit;

/** {1:pp Pretty printing} */;

let pp: (Format.formatter, t('a)) => unit;

let pp_re: (Format.formatter, re('a)) => unit;

/**/**/;

/** Internal types */

module Internal: {
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

  let from_t: t('a) => raw('a);
  let to_t: raw('a) => t('a);

  type wit(_) =
    | Lit(int): wit(string)
    | Conv(wit('a), conv('a, 'b)): wit('b)
    | Opt(Re.Mark.t, wit('a)): wit(option('a))
    | Alt(Re.Mark.t, wit('a), wit('b)): wit([ | `Left('a) | `Right('b)])
    | Seq(wit('a), wit('b)): wit(('a, 'b))
    | Rep(int, wit('a), Re.re): wit(Seq.t('a));

  let build: (int, raw('a)) => (int, wit('a), Re.t);
  let extract: (~original: string, wit('a), Re.Group.t) => 'a;
};

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
