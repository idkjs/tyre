[@ocaml.warning "-40-44-48"];

let length = seq => {
  let rec aux = (acc, seq) =>
    switch (seq()) {
    | Seq.Nil => acc
    | [@implicit_arity] Cons(_, l) => aux(acc + 1, l)
    };

  aux(0, seq);
};

let re = Tyre.compile(Tyre.(whole_string @@ rep(RFC2616.request)));
let tyre = s =>
  switch (Tyre.exec(re, s)) {
  | Result.Ok(l) => assert(length(l) == 55 * 100)
  | Result.Error(_) => failwith("oups")
  };

let tyre_test = s => assert(Tyre.execp(re, s));

let re2 = Tyre.compile(RFC2616.request);
let tyre_all = s =>
  switch (Tyre.all(re2, s)) {
  | Result.Ok(l) => assert(List.length(l) == 55 * 100)
  | Result.Error(_) => failwith("oups")
  };
let tyre_all_seq = s => {
  let l = Tyre.all_seq(re2, s);
  assert(length(l) == 55 * 100);
};

let angstrom = s =>
  switch (Angstrom.(parse_string(many(Angstrom_rFC2616.request)))(s)) {
  | Result.Ok(l) => assert(List.length(l) == 55 * 100)
  | Result.Error(_) => failwith("oups")
  };

let oc = open_in("benchmark/data/http-requests.txt.100");
let s = CCIO.read_all(oc);

let l = [
  ("tyre", tyre),
  ("tyre.all", tyre_all),
  ("tyre.all_seq", tyre_all_seq),
  ("tyre.test", tyre_test),
  ("angstrom", angstrom),
];

/** Utilities */;

let benchs = {
  let get_sample = ((name, f)) => (
    name,
    lazy(Benchmark.latency1(~style=Nil, ~name, ~repeat=4, 50L, f, s)),
  );

  List.map(get_sample, l);
};

let tree = {
  open Benchmark.Tree;
  let f = ((n, s)) => n @> s;
  concat @@ List.map(f, benchs);
};

let samples = () => {
  let f = ((_, s), l) =>
    if (Lazy.is_val(s)) {
      Benchmark.merge(Lazy.force(s), l);
    } else {
      l;
    };

  List.fold_right(f, benchs, []);
};

let () = {
  open Benchmark;
  let tree = Tree.("http" @>> tree);
  Tree.register(tree);
  Tree.run_global();
  let l = samples();
  if (l != []) {
    tabulate(l);
  };
};
