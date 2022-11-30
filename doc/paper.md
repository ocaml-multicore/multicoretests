Multicoretests - Parallel Testing Libraries for OCaml 5.0
=========================================================

**Jan Midtgaard,   Olivier Nicole, and   Nicolas Osborne**, *Tarides*


Introduction
------------

Parallel and concurrent code is notoriously hard to test because of
the involved non-determinism, yet it is facing OCaml programmers with
the coming OCaml 5.0 multicore release. We present two related testing
libraries to improve upon the situation:

- `Lin` - a library to test for sequential consistency
- `STM` - a state-machine testing library

Both libraries build on [QCheck][qcheck], a black-box, property-based
testing library in the style of [QuickCheck](#QuickCheck). The two
libraries represent different trade-offs between required user effort
and provided guarantees and thereby supplement each other.

In this document we will use OCaml's `Hashtbl` module as a running
example.


The `Lin` library
-----------------

The `Lin` library performs a sequence of random operations in
parallel, records the results, and checks whether the observed results
are linearizable by reconciling them with a sequential execution.

The library offers an embedded, combinator DSL to describe signatures
succinctly. As an example, the required specification to test (parts
of) the `Hashtbl` module is as follows:

```ocaml
module HashtblSig =
struct
  type t = (char, int) Hashtbl.t
  let init () = Hashtbl.create ~random:false 42
  let cleanup _ = ()

  open Lin
  let a,b = char_printable,nat_small
  let api =
    [ val_ "Hashtbl.clear"    Hashtbl.clear    (t @-> returning unit);
      val_ "Hashtbl.add"      Hashtbl.add      (t @-> a @-> b @-> returning unit);
      val_ "Hashtbl.remove"   Hashtbl.remove   (t @-> a @-> returning unit);
      val_ "Hashtbl.find"     Hashtbl.find     (t @-> a @-> returning_or_exc b);
      val_ "Hashtbl.replace"  Hashtbl.replace  (t @-> a @-> b @-> returning unit);
      val_ "Hashtbl.mem"      Hashtbl.mem      (t @-> a @-> returning bool);
      val_ "Hashtbl.length"   Hashtbl.length   (t @-> returning int); ]
end
```
The first line indicates the type of the system under test (SUT). In the
above case we intend to test `Hashtbl`s with `char` keys and `int`
values. The bindings `init` and `cleanup` allow for setting up and
tearing down the SUT. The `api` then contains a list of type signature
descriptions using combinators in the style of [Ctypes][ctypes]. Different
combinators `unit`, `bool`, `int`, `list`, `option`, `returning`,
`returning_or_exc`, ... allow for a concise type signature description.

From the above description the `Lin` library will iterate a number of
test instances. Each test instance consists of a "sequential prefix"
of calls to the above operations, followed by a `spawn` of two
parallel `Domain`s that each call a sequence of operations.

For each test instance `Lin` chooses the individual operations
arbitrarily and records the result received from each operation. The
framework will then perform a search for a sequential interleaving of
the same calls, and succeed if it finds one. Since `Hashtbl`s are not
safe for parallelism, the output produces the following:

```
  Results incompatible with sequential execution

                                    |
                        Hashtbl.add t '@' 4  : ()
                                    |
                 .------------------------------------.
                 |                                    |
     Hashtbl.add t '.' 3  : ()              Hashtbl.clear t  : ()
                                            Hashtbl.length t  : 2
```

This describes that in one parallel execution, `Lin` received the
response `2` from `Hashtbl.length`, despite having just executed
`Hashtbl.clear`.  It this case, it is not possible to interleave
`Hashtbl.add t '.' 3` with these two calls to explain this observed
behaviour.

Underneath the hood, `Lin` does its best to schedule the two parallel
`Domain`s on top of each other. It also repeats each test instance, to
increase the chance of triggering an error, and it fails if just one
of the repetitions fail to find a sequential interleaving. Finally,
upon finding an error it reduces the involved operation sequences to a
local minimum, which is what is printed above.

`Lin` is phrased as an OCaml functor, `Lin_domain.Make`. The module
resulting from `Lin_domain.Make(HashtblSig)` contains a binding `lin_test`
that can perform the above linearization test over `Domain`s, the
basic unit of parallelism coming in OCaml 5.0. An alternative `Lin`
mode works over `Thread` for testing concurrent but non-overlapping
executions. This mode thus mimicks the above functionality by
replacing `Domain.spawn` and `Domain.join` with `Thread.create` and
`Thread.join`, respectively.


The `STM` library
-----------------

Like `Lin` the `STM` library also performs a sequence of random
operations in parallel and records the results. In contrast to `Lin`,
`STM` then checks whether the observed results are linearizable by
reconciling them with a sequential execution of a `model` description.

The `model` expresses the intended meaning of each tested
operation. As such, the required `STM` user input is longer compared
to that of `Lin`. The corresponding code to describe a `Hashtbl`
test using `STM` is given below:

```ocaml
module HashtblModel =
struct
  type sut = (char, int) Hashtbl.t
  type state = (char * int) list
  type cmd =
    | Clear
    | Add of char * int
    | Remove of char
    | Find of char
    | Replace of char * int
    | Mem of char
    | Length [@@deriving show { with_path = false }]

  let init_sut () = Hashtbl.create ~random:false 42
  let cleanup (_:sut) = ()

  let arb_cmd (s:state) =
    let char =
      if s = []
      then Gen.printable
      else Gen.(oneof [oneofl (List.map fst s);
                       printable]) in
    let int = Gen.nat in
    QCheck.make ~print:show_cmd
     (Gen.oneof
       [Gen.return Clear;
        Gen.map2 (fun k v -> Add (k,v)) char int;
        Gen.map  (fun k   -> Remove k) char;
        Gen.map  (fun k   -> Find k) char;
        Gen.map2 (fun k v -> Replace (k,v)) char int;
        Gen.map  (fun k   -> Mem k) char;
        Gen.return Length;
       ])

  let next_state (c:cmd) (s:state) = match c with
    | Clear         -> []
    | Add (k,v)     -> (k,v)::s
    | Remove k      -> List.remove_assoc k s
    | Find _        -> s
    | Replace (k,v) -> (k,v)::(List.remove_assoc k s)
    | Mem _
    | Length        -> s

  let run (c:cmd) (h:sut) = match c with
    | Clear         -> Res (unit, Hashtbl.clear h)
    | Add (k,v)     -> Res (unit, Hashtbl.add h k v)
    | Remove k      -> Res (unit, Hashtbl.remove h k)
    | Find k        -> Res (result int exn,
                             protect (Hashtbl.find h) k)
    | Replace (k,v) -> Res (unit, Hashtbl.replace h k v)
    | Mem k         -> Res (bool, Hashtbl.mem h k)
    | Length        -> Res (int,  Hashtbl.length h)

  let init_state = []

  let precond (_:cmd) (_:state) = true
  let postcond (c:cmd) (s:state) (res:res) =
    match c,res with
    | Clear,         Res ((Unit,_),_)
    | Add (_,_),     Res ((Unit,_),_)
    | Remove _,      Res ((Unit,_),_) -> true
    | Find k,        Res ((Result (Int,Exn),_),r) ->
        r = (try Ok (List.assoc k s)
             with Not_found -> Error Not_found)
    | Replace (_,_), Res ((Unit,_),_) -> true
    | Mem k,         Res ((Bool,_),r) -> r = List.mem_assoc k s
    | Length,        Res ((Int,_),r)  -> r = List.length s
    | _ -> false
```

Again this requires a description of the system under test, `sut`. In
addition `STM` requires a type `cmd` for describing the tested
operations. The hooks `init_sut` and `cleanup` match `init` and
`cleanup` from `Lin`, respectively.

A distinguishing feature is `type state = (char * int) list`
describing with a pure association list the internal state of a
`Hashtbl`. `next_state` is a simple state transition function
describing how the `state` changes across each `cmd`. For example,
`Add (k,v)` appends the key-value pair onto the association list.

`arb_cmd` is a generator of `cmd`s, taking `state` as a parameter.
This allows for `state`-dependent `cmd` generation, which we use
to increase the chance of producing a `Remove 'c'`, `Find 'c'`, ...
following an `Add 'c'`. Internally `arb_cmd` uses combinators
`Gen.return`, `Gen.map`, and `Gen.map2` from QCheck to generate one of
7 different operations. For example, `Gen.map (fun k -> Mem k) char`
creates a `Mem` command with the result obtained from the `char`
generator. `arb_cmd` further uses a derived printer `show_cmd` to
be able to print a counterexample.

`run` executes the tested `cmd` over the SUT and wraps the result up
in a result type `res` offered by `STM`. Combinators `unit`, `bool`,
`int`, ... allow to annotate the result with the expected type.
`postcond` then expresses a post-condition by matching the received
`res`, for a given `cmd` with the corresponding answer from the
`model` description. For example, this compares the Boolean result `r`
from `Hashtbl.mem` with the result from `List.mem_assoc`. Similarly
`precond` expresses a `cmd` pre-condition.


`STM` is also phrased as an OCaml functor. The module resulting from
`STM_domain.Make(HashtblModel)` thus includes a binding
`agree_test` for running sequential tests comparing the SUT
behaviour to the given model.
Another binding `agree_test_par` instead runs parallel tests that make a similar
comparison over a sequential prefix and two parallel `Domain`s, this
time also searching for a sequential interleaving of `cmd`s.
For example, one execution of `agree_test_par` produced the following
output. Note how no interleaving of `Remove` from the first parallel
`cmd` sequence can make the association list model return `-1` from
`Length`:

```
    Results incompatible with linearized model

                          |
                (Add ('1', 5)) : ()
                          |
              .-----------------------.
              |                       |
      (Remove '1') : ()           Clear : ()
                                 Length : -1
```


Status
------

Both libraries are open source and available for download on GitHub
from https://github.com/jmid/multicoretests
As the APIs are still unstable and under development, we have not made
a public release yet. Interested users can nevertheless easily install
the libraries with `opam`.

During development we have used examples such as `Hashtbl` to
confirm that the approach indeed works as intended. The behaviour is
continuously confirmed by running GitHub Actions of the latest trunk
compiler. As further testament to the usability of the approach, we
have used the libraries to test parts of OCaml's `Stdlib`, as well as
the `Domainslib` and `lockfree` libraries. In doing so, we have been
able to find and report a number of issues which have either already
been fixed or have fixes underway:

- `In_channel` and `Out_channel` unsafety https://github.com/jmid/multicoretests/pull/13 https://github.com/ocaml/ocaml/issues/10960#issuecomment-1087660763
- MacOSX crash - https://github.com/ocaml/ocaml/issues/11226
- `Buffer` unsafety - https://github.com/jmid/multicoretests/pull/63 https://github.com/ocaml/ocaml/issues/11279


Related work
------------

- [QuickCheck](#QuickCheck) originally introduced property-based
  testing within functional programming with combinator-based
  generators, properties, and test-case reduction. It has since been
  ported to over 30 other programming languages, including [Quviq
  QuickCheck][Quviq QuickCheck] - a commercial port to Erlang.

- Model-based testing was initially suggested as a method for [testing
  monadic code with Haskell's QuickCheck](#Haskell-model). An explicit
  framework was later proposed in [the GAST property-based testing
  library for Clean](#Gast-Clean). The commercial [Quviq QuickCheck][Quviq QuickCheck]
  was later extended with a [state-machine model framework for testing stateful systems](#Erlang-eqc_commands).
  This approach was extended further to [test parallel code for data races](#Erlang-eqc_par_statem).
  This general approach for parallel testing has since been adopted in
  other ports, such as Erlang's open source [Proper][Proper], Haskell
  [Hedgehog][hedgehog], [ScalaCheck][scalacheck], and Kotlin's
  [propCheck][propcheck]. `STM` continues this adoption tradition.
  [qcstm][qcstm] is a previous OCaml adoption, also building on QCheck.
  It was missing the ability to perform parallel testing though.
  `STM` seeks to remedy this limitation.

- [Crowbar][crowbar] is another QuickCheck-style
  testing framework with combinator-based generators.  In contrast to
  QuickCheck, it utilizes AFL-based coverage guidance to effectively
  guide the generated input towards unvisited parts of the SUT. Crowbar
  does not come with a state-machine framework.
  [Monolith](#Monolith) is a
  model-based testing framework also building on AFL-based coverage
  guidance. In contrast to `STM`, Monolith's models are oracle
  implementations with operations matching the type signatures of the
  tested operations. Neither Crowbar nor Monolith come with skeletons to
  perform parallel or concurrent testing. Furthermore the AFL-based
  coverage-guidance underlying both Crowbar and Monolith works best for
  deterministic, sequential code.

- [ParaFuzz][parafuzz] is another approach to fuzz test multicore OCaml programs.
  It simulates parallelism in OCaml through concurrency, enabling
  scheduling order to be controlled by AFL, which helps to trigger and
  find scheduling-dependent bugs. A caveat is that ParaFuzz assumes data race freedom.

- [Ortac][ortac] can extract Monolith-based tests from a formal specification
  written in Gospel, a specification language for OCaml.
  Gospel specifications include models, pre-conditions, and
  post-conditions close to those of `STM`. The extracted tests
  however inherit Monolith's and AFL's focus on sequential code.

- [ArtiCheck][articheck] tests random combinations of OCaml calls from
  type signature descriptions, similarly to `Lin`. Whereas `Lin` and
  `STM` target impure interfaces, ArtiCheck targets both persistent
  (pure) interfaces. ArtiCheck furthermore targets sequential rather
  than parallel or concurrent tests.


Conclusion
----------

We have presented two libraries, `Lin` and `STM` for testing parallel
and concurrent code for OCaml 5.0. Despite still being under
development, we believe both libraries could be helpful to developers
of OCaml 5.0 programs.


References
----------

[qcheck]:                https://github.com/c-cube/qcheck

##### QuickCheck
Koen Claessen and John Hughes, QuickCheck: A Lightweight Tool for Random Testing of Haskell Programs, ICFP 2000
https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf

##### ctypes
Jeremy Yallop, David Sheets, and Anil Madhavapeddy, A modular foreign function interface, Science of Computer Programming, vol.164, 2018
https://anil.recoil.org/papers/2018-socp-modular-ffi.pdf
https://github.com/ocamllabs/ocaml-ctypes

[Quviq QuickCheck]:      http://quviq.com/documentation/eqc/index.html

##### Haskell-model
Koen Claessen and John Hughes, Testing Monadic Code with QuickCheck, Haskell 2002
https://www.cs.tufts.edu/~nr/cs257/archive/koen-claessen/quickmonad.ps

##### Gast-Clean
Pieter Koopman and Rinus Plasmeijer, Testing reactive systems with GAST, TFP 2003,
https://www.mbsd.cs.ru.nl/publications/papers/2004/koop2004-TestReactiveGAST.pdf
http://www.cs.ru.nl/~pieter/gentest/gentest.html

##### Erlang-eqc_commands
Thomas Arts, John Hughes, Joakim Johansson, and Ulf Wiger, Testing Telecoms Software with Quviq QuickCheck, Erlang 2006,
http://www.quviq.com/wp-content/uploads/2014/08/erlang001-arts.pdf

##### Erlang-eqc_par_statem
Koen Claessen et al., Finding Race Conditions in Erlang with QuickCheck and PULSE, ICFP 2009
https://smallbone.se/papers/finding-race-conditions.pdf

##### Proper
Manolis Papadakis and Konstantinos Sagonas, A PropEr Integration of Types and Function Specifications with Property-Based Testing, Erlang 2011
https://proper-testing.github.io/papers/proper_types.pdf
https://github.com/proper-testing/proper

[hedgehog]:              https://github.com/hedgehogqa/haskell-hedgehog

[scalacheck]:            https://github.com/typelevel/scalacheck

[propcheck]:             https://github.com/1Jajen1/propCheck

##### qcstm
Jan Midtgaard, A Simple State-Machine Framework for Property-Based Testing in OCaml, OCaml Users and Developers Workshop 2020
https://janmidtgaard.dk/papers/Midtgaard%3AOCaml20.pdf
https://github.com/jmid/qcstm

##### crowbar
Stephen Dolan and Mindy Preston, Testing with Crowbar, OCaml Users and Developers Workshop 2017
https://67.207.157.55/meetings/ocaml/2017/extended-abstract__2017__stephen-dolan_mindy-preston__testing-with-crowbar.pdf
https://github.com/stedolan/crowbar

##### Mmonolith
François Pottier, Strong Automated Testing of OCaml Libraries, JPFL 2021,
http://cambium.inria.fr/~fpottier/publis/pottier-monolith-2021.pdf
https://gitlab.inria.fr/fpottier/monolith

##### parafuzz
Sumit Padhiyar, Adharsh Kamath, and KC Sivaramakrishnan, Parafuzz: Coverage-guided Property Fuzzing for Multicore OCaml Programs, OCaml Users and Developers Workshop 2021
https://github.com/ocaml-multicore/parafuzz

##### ortac
Nicolas Osborne and Clément Pascutto, Leveraging Formal Specifications to Generate Fuzzing Suites, OCaml Users and Developers Workshop 2021
https://hal.inria.fr/hal-03328646/file/OCaml_2021.pdf
https://github.com/ocaml-gospel/ortac

##### articheck
Thomas Braibant, Jonathan Protzenko, and Gabriel Scherer, Well-Typed Generic Smart Fuzzing for APIs, ML Familiy Workshop 2014
https://hal.inria.fr/hal-01094006/document
https://github.com/braibant/articheck
