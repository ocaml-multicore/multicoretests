(** {1 Type-representing values} *)

type constructible = |
type deconstructible = |
type combinable
type noncombinable

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
val nat64 : (Int64.t, 'a, 'b, combinable) ty
val nat64_small : (Int64.t, 'a, 'b, combinable) ty
val float : (float, 'a, 'b, combinable) ty
val string : (String.t, 'a, 'b, combinable) ty

val option :
  ?ratio:float ->
  ('a, 'c, 's, combinable) ty -> ('a option, 'c, 's, combinable) ty
val opt :
  ?ratio:float ->
  ('a, 'b, 'c, combinable) ty -> ('a option, 'b, 'c, combinable) ty
val list : ('a, 'c, 's, combinable) ty -> ('a list, 'c, 's, combinable) ty
val state : ('a, constructible, 'a, noncombinable) ty
val t : ('a, constructible, 'a, noncombinable) ty
val print_result :
  ('a -> string) -> ('b -> string) -> ('a, 'b) result -> string
val or_exn :
  ('a, deconstructible, 'b, combinable) ty ->
  (('a, exn) result, deconstructible, 'c, combinable) ty

(** Given a description of type ['a], print a value of type ['a]. *)
val print : ('a, 'c, 's, 'comb) ty -> 'a -> string

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
val returning_or_exc :
  ('a, deconstructible, 'b, combinable) ty ->
  ('a, ('a, exn) result, 'b) Fun.fn
val returning_ : ('a, 'b, 'c, combinable) ty -> ('a, unit, 'c) Fun.fn
val returning_or_exc_ :
  ('a, 'b, 'c, combinable) ty -> ('a, (unit, exn) result, 'c) Fun.fn

val ( @-> ) :
  ('a, constructible, 'b, 'c) ty ->
  ('d, 'e, 'b) Fun.fn -> ('a -> 'd, 'e, 'b) Fun.fn


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
