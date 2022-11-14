(** The [Lin_api] module allows the user to describe the type signature of
    a tested module interface using a DSL of type combinators.
*)

(** {1 Type-representing values} *)

type constructible = |
(** Type definition to denote whether a described type can be generated *)

type deconstructible = |
(** Type definition to denote whether a described type can be deconstructed,
    i.e., tested for equality. *)

type combinable
(** Type definition to denote that a described type can be composed with
    other combinators such as [list]. *)

type noncombinable
(** Type definition to denote that a described type cannot be composed with
    other combinators such as [list]. *)

type (_, _, _, _) ty
(** Type definition for type-describing combinators.
    [(typ,con,styp,comb) ty] represents a type [typ] and with an underlying state of type [styp].
    The [con] type parameter indicates whether the combinator type is [constructible] or [deconstructible].
    The [comb] type parameter indicates whether the combinator type is [combinable] or [noncombinable].
 *)

val gen : 'a QCheck.arbitrary -> ('a -> string) -> ('a, constructible, 's, combinable) ty
(** [gen arb to_str] builds a [constructible] and [combinable] type combinator
    from a QCheck generator [arb] and a to-string function [to_str]. *)

val deconstructible : ('a -> string) -> ('a -> 'a -> bool) -> ('a, deconstructible, 's, combinable) ty
(** [deconstructible to_str eq] builds a [deconstructible] and [combinable] type combinator
    from a to-string function [to_str] and an equality predicate [eq]. *)

val gen_deconstructible : 'a QCheck.arbitrary -> ('a -> string) -> ('a -> 'a -> bool) -> ('a, 'c, 's, combinable) ty
(** [gen_deconstructible arb to_str eq] builds a [combinable] type combinator
    from a QCheck generator [arb], a to-string function [to_str] and an
    equality predicate [eq]. *)

val unit : (unit, 'a, 'b, combinable) ty
(** The [unit] combinator represents the [unit] type *)

val bool : (bool, 'a, 'b, combinable) ty
(** The [bool] combinator represents the [bool] type *)

val char : (char, 'a, 'b, combinable) ty
(** The [char] combinator represents the [char] type.
    It uses a uniform generator based on [QCheck.char]. *)

val char_printable : (char, 'a, 'b, combinable) ty
(** The [char_printable] combinator represents the [char] type.
    The generated characters have character codes 32-126 or 10 (newline)
    and are based on [QCheck.printable_char]. *)

val nat_small : (int, 'a, 'b, combinable) ty
(** The [nat_small] combinator represents the [int] type.
    The generated integers are non-negative, less than 100,
    and are based on [QCheck.small_nat]. *)

val int : (int, 'a, 'b, combinable) ty
(** The [int] combinator represents the [int] type.
    It uses a uniform generator based on [QCheck.int]. *)

val int_small : (int, 'a, 'b, combinable) ty
(** The [int_small] combinator represents the [int] type.
    The generated integers are non-negative (unsigned)
    and are based on [QCheck.small_int]. *)

val int_pos : (int, 'a, 'b, combinable) ty
(** The [int_pos] combinator represents the [int] type.
    The generated integers are non-negative (unsigned)
    and uniformly distributed. It is based on [QCheck.pos_int]. *)

val int_bound : int -> (int, 'a, 'b, combinable) ty
(** The [int_bound b] combinator represents the [int] type.
    The generated integers are range between [0] and [b], inclusive.
    It uses a uniform generator based on [QCheck.int_bound]. *)

val int32 : (Int32.t, 'a, 'b, combinable) ty
(** The [int32] combinator represents the [int32] type.
    It uses a uniform generator based on [QCheck.int32]. *)

val int64 : (Int64.t, 'a, 'b, combinable) ty
(** The [int64] combinator represents the [int64] type.
    It uses a uniform generator based on [QCheck.int64]. *)

val nat64_small : (Int64.t, 'a, 'b, combinable) ty
(** The [nat64_small] combinator represents the [int64] type.
    The generated integers are non-negative (unsigned)
    and are based on [QCheck.small_nat]. *)

val float : (float, 'a, 'b, combinable) ty
(** The [float] combinator represents the [float] type.
    The generated floating point numbers do not include nan and infinitites,
    It is based on [QCheck.float]. *)

val string : (String.t, 'a, 'b, combinable) ty
(** The [string] combinator represents the [string] type.
    The generated strings have a size generated from [QCheck.Gen.nat] and
    characters resulting from [QCheck.char]. It is based on [QCheck.string]. *)

val string_small : (String.t, 'a, 'b, combinable) ty
(** The [small_string] combinator represents the [string] type.
    The generated strings have a size generated from [QCheck.Gen.small_nat] and
    characters resulting from [QCheck.char]. It is based on [QCheck.small_string]. *)

val string_small_printable : (String.t, 'a, 'b, combinable) ty
(** The [small_string] combinator represents the [string] type.
    The generated strings have a size generated from [QCheck.Gen.small_nat] and
    characters resulting from [QCheck.printable_char].
    It is based on [QCheck.small_printable_string]. *)

val option :
  ?ratio:float ->
  ('a, 'c, 's, combinable) ty -> ('a option, 'c, 's, combinable) ty
(** The [option] combinator represents the [option] type.
    The generated values from [option t] are either [Some v] or [None]
    with [v] being generated by the [t] combinator.
    An optional [ratio] allow to change the default [0.85] [Some]s.
    It is based on [QCheck.option]. *)

val opt :
  ?ratio:float ->
  ('a, 'b, 'c, combinable) ty -> ('a option, 'b, 'c, combinable) ty
(** The [opt] combinator is an alias for [option]. *)

val list : ('a, 'c, 's, combinable) ty -> ('a list, 'c, 's, combinable) ty
(** The [list] combinator represents the [list] type.
    The generated lists from [list t] have a length resulting from [QCheck.Gen.nat]
    and have their elements generated by the [t] combinator.
    It is based on [QCheck.list]. *)

val array : ('a, 'c, 's, combinable) ty -> ('a array, 'c, 's, combinable) ty
(** The [array] combinator represents the [array] type.
    The generated arrays from [array t] have a length resulting from [QCheck.Gen.nat]
    and have their elements generated by the [t] combinator.
    It is based on [QCheck.array]. *)

val seq : ('a, 'c, 's, combinable) ty -> ('a Seq.t, 'c, 's, combinable) ty
(** The [seq] combinator represents the [Seq.t] type.
    The generated sequences from [seq t] have a length resulting from [QCheck.Gen.nat]
    and have their elements generated by the [t] combinator. *)

val t : ('a, constructible, 'a, noncombinable) ty
(** The [t] combinator represents the type [ApiSpec.t] of the system under test. *)

val state : ('a, constructible, 'a, noncombinable) ty
(** The [state] combinator represents the type [ApiSpec.t] of the system under test.
    It is an alias for the [t] combinator. *)

val or_exn :
  ('a, deconstructible, 'b, combinable) ty ->
  (('a, exn) result, deconstructible, 'c, combinable) ty
(** The [or_exn] combinator transforms a result type representing [t]
    into a [(t, exn) result] type. *)

val print_result :
  ('a -> string) -> ('b -> string) -> ('a, 'b) result -> string
(** [print_result pa pb] creates a to-string function for a [(a,b) result] type
    given two to-string functions for [a]s and [b]s, respectively. *)

val print : ('a, 'c, 's, 'comb) ty -> 'a -> string
(** Given a description of type ['a], print a value of type ['a]. *)

val equal : ('a, deconstructible, 's, 'comb) ty -> 'a -> 'a -> bool
(** Given a description of type ['a], compare two values of type ['a]. *)


(** {1 Values representing API functions} *)

module Fun : sig
  (** [(ftyp,rtyp,styp) Fun.fn] represents a function type of type [ftyp], with return type [rtyp],
      and with the underlying state type [styp]. *)
  type (_, _, _) fn
end

val returning :
  ('a, deconstructible, 'b, combinable) ty -> ('a, 'a, 'b) Fun.fn
(** [returning t] represents a pure return type. *)

val returning_or_exc :
  ('a, deconstructible, 'b, combinable) ty ->
  ('a, ('a, exn) result, 'b) Fun.fn
(** [returning_or_exc t] represents a return type of a function that may raise an exception. *)

val returning_ : ('a, 'b, 'c, combinable) ty -> ('a, unit, 'c) Fun.fn
(** [returning_ t] represents a return type that should be ignored. *)

val returning_or_exc_ :
  ('a, 'b, 'c, combinable) ty -> ('a, (unit, exn) result, 'c) Fun.fn
(** [returning_or_exc_ t] represents a return type that should be ignored of a function
    that may raise an exception. *)

val ( @-> ) :
  ('a, constructible, 'b, 'c) ty ->
  ('d, 'e, 'b) Fun.fn -> ('a -> 'd, 'e, 'b) Fun.fn
(** [at @-> rt] represents a function type expecting an argument [at]
    and returning [rt]. *)


(** {1 API description} *)

(** Type and constructor to capture a single function signature *)
type _ elem = private
  | Elem : string * ('ftyp, 'r, 's) Fun.fn * 'ftyp -> 's elem

type 's api = (int * 's elem) list
(** The type of module signatures *)

val val_ : string -> 'f -> ('f, 'r, 's) Fun.fn -> int * 's elem
(** [val_ str f sig] describes a function signature from a string [str],
    a function value [f], and a signature description [sig]. *)

val val_freq : int -> string -> 'f -> ('f, 'r, 's) Fun.fn -> int * 's elem
(** [val_ freq w str f sig] describes a function signature like
    [val_ str f sig] but with weight [w]. *)


(** The required description of a module signature *)
module type ApiSpec =
  sig
    type t
    (** The type of the system under test *)

    val init : unit -> t
    (** The function to initialize the system under test *)

    val cleanup : t -> unit
    (** The function to cleanup the system under test *)

    val api : (int * t elem) list
    (** A description of the function signatures *)
  end


(** {1 Generation of linearizability testing module from an API} *)

module MakeCmd : functor (Spec : ApiSpec) -> Lin_internal.CmdSpec
(** Functor to map a combinator-based module signature description
    into a raw [Lin] description *)
