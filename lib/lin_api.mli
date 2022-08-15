(** {1 Type-representing values} *)

type constructible = |
type deconstructible = |
type combinable
type noncombinable

(** The (_,_,_,_) ty describes the type of type combinators.
   These are accepted by the [@->], [returning], [returning_], [returning_or_exc], and [returning_or_exc_] combinators
   The four type parameters:
   - 1st: the type of the produced values
   - 2nd: indicating whether the type is constructible (generatable) or deconstructible by equality testing
   - 3rd: indicating the type of the underlying state
   - 4th: indicating whether the type is combinable with other combinators, e.g., [list].
          Note: [state]/[t] is currently [noncombinable].
*)
type (_, _, _, _) ty

val gen : 'a QCheck.arbitrary -> ('a -> string) -> ('a, constructible, 's, combinable) ty
val deconstructible : ('a -> string) -> ('a -> 'a -> bool) -> ('a, deconstructible, 's, combinable) ty
val gen_deconstructible : 'a QCheck.arbitrary -> ('a -> string) -> ('a -> 'a -> bool) -> ('a, 'c, 's, combinable) ty

val unit : (unit, 'a, 'b, combinable) ty
val bool : (bool, 'a, 'b, combinable) ty
val char : (char, 'a, 'b, combinable) ty
val char_printable : (char, 'a, 'b, combinable) ty
val nat_small : (int, 'a, 'b, combinable) ty
val int : (int, 'a, 'b, combinable) ty
val int_small : (int, 'a, 'b, combinable) ty
val int_pos : (int, 'a, 'b, combinable) ty
val int_bound : int -> (int, 'a, 'b, combinable) ty
val int32 : (Int32.t, 'a, 'b, combinable) ty
val int64 : (Int64.t, 'a, 'b, combinable) ty
val nat64_small : (Int64.t, 'a, 'b, combinable) ty
val float : (float, 'a, 'b, combinable) ty
val string : (String.t, 'a, 'b, combinable) ty
val string_small : (String.t, 'a, 'b, combinable) ty
val string_small_printable : (String.t, 'a, 'b, combinable) ty

val option :
  ?ratio:float ->
  ('a, 'c, 's, combinable) ty -> ('a option, 'c, 's, combinable) ty
val opt :
  ?ratio:float ->
  ('a, 'b, 'c, combinable) ty -> ('a option, 'b, 'c, combinable) ty
val list : ('a, 'c, 's, combinable) ty -> ('a list, 'c, 's, combinable) ty
val array : ('a, 'c, 's, combinable) ty -> ('a array, 'c, 's, combinable) ty (* FIXME: shouldn't be deconstructible? *)
val seq : ('a, 'c, 's, combinable) ty -> ('a Seq.t, 'c, 's, combinable) ty

val state : ('a, constructible, 'a, noncombinable) ty
val t : ('a, constructible, 'a, noncombinable) ty
val print_result :
  ('a -> string) -> ('b -> string) -> ('a, 'b) result -> string
val or_exn :
  ('a, deconstructible, 'b, combinable) ty ->
  (('a, exn) result, deconstructible, 'c, combinable) ty

(** Given a description of type ['a], print a value of type ['a]. *)
val print : ('a, 'c, 's, combinable) ty -> 'a -> string

(** Given a description of type ['a], compare two values of type ['a]. *)
val equal : ('a, deconstructible, 's, 'comb) ty -> 'a -> 'a -> bool


(** {1 Values representing API functions} *)

module Fun : sig
  (** The type arguments are: the function type, the return type, and the type
      of the underlying state. *)
  type (_, _, _) fn
end

val returning :
  ('a, deconstructible, 'b, combinable) ty -> ('a, 'a, 'b) Fun.fn
(** [returning comb] indicates the return type [comb] to be compared to the corresponding sequential result.
    For this reason [comb] has to be [deconstructible]. *)

val returning_or_exc :
  ('a, deconstructible, 'b, combinable) ty ->
  ('a, ('a, exn) result, 'b) Fun.fn
(** [returning_or_exc comb] indicates that the function may raise an exception and that the return type is [comb].
    In either case the result is compared to the corresponding sequential result and hence [comb] has to be [deconstructible]. *)

val returning_ : ('a, 'b, 'c, _) ty -> ('a, unit, 'c) Fun.fn
(** [returning comb] indicates the return type [comb] which is ignored. *)

val returning_or_exc_ :
  ('a, 'b, 'c, _) ty -> ('a, (unit, exn) result, 'c) Fun.fn
(** [returning_or_exc comb] indicates that the function may raise an exception and that the return type is [comb].
    In both cases the result is ignored. *)

val ( @-> ) :
  ('a, constructible, 'b, 'c) ty ->
  ('d, 'e, 'b) Fun.fn -> ('a -> 'd, 'e, 'b) Fun.fn
(** [arg_typ @-> res_typ] indicates a function signature expecting [arg_typ] and returning [res_typ].
    Note: eventually has to end in one of the [returning] combinators. *)

(** {1 API description} *)

type _ elem = private
  | Elem : string * ('ftyp, 'r, 's) Fun.fn * 'ftyp -> 's elem

type 's api = (int * 's elem) list

val val_ : string -> 'f -> ('f, 'r, 's) Fun.fn -> int * 's elem
val val_with_freq : int -> string -> 'f -> ('f, 'r, 's) Fun.fn -> int * 's elem

module type ApiSpec =
  sig
    type t
    val init : unit -> t
    val cleanup : t -> unit
    val api : (int * t elem) list
  end


(** {1 Generation of linearizability testing module from an API} *)

module MakeCmd : functor (ApiSpec : ApiSpec) -> Lin.CmdSpec

module Make :
  functor (ApiSpec : ApiSpec) -> module type of Lin.Make (MakeCmd (ApiSpec))
