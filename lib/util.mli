(** The Util module contains a number of reusable functions
    handy for multicore testing. *)


val repeat : int -> ('a -> bool) -> 'a -> bool
(** [repeat num prop] iterates a property [prop] [num] times. The function stops
    early and returns false if just one of the iterations returns false.
    This is handy if the property outcome is non-determistic, for example,
    if it depends on scheduling. *)

exception Timeout
(** exception raised by [prop_timeout] and [fork_prop_with_timeout]. *)

val prop_timeout : int -> ('a -> 'b) -> 'a -> 'b
(** [prop_timeout s prop] returns a property working as [prop] that times out and
    raises [Timeout] after [s] seconds. *)

val fork_prop_with_timeout : int -> ('a -> bool) -> 'a -> bool
(** [fork_prop_with_timeout s prop] tests a property in a separate process and
    times out and raises [Timeout] after [s] seconds, like [prop_timeout s prop].
    This is handy if the tested code can segfault or loop infinitely. *)

val print_vertical : ?fig_indent:int -> ('a -> string) -> 'a list -> string
(** [print_vertical pr cmds] returns a string representing a sequential trace.
    Optional [fig_indent] indicates how many spaces it should be indented (default: 3 spaces).
 *)

val print_triple_vertical :
  ?fig_indent:int ->
  ?res_width:int ->
  ?center_prefix:bool ->
  ('a -> string) -> 'a list * 'a list * 'a list -> string
(** [print_triple_vertical pr (xs,ys,zs)] returns a string representing a
    parallel trace, with [xs] printed first, and then [ys] and [zs] printed
    in parallel.
    Optional [fig_indent] indicates how many spaces it should be indented (default: 10 spaces).
    Optional [res_width] specifies the reserved width for printing each list entry (default: 20 chars).
    Optional [center_prefix] centers the sequential prefix if [true] (the default) and otherwise left-adjust it.
 *)

val protect : ('a -> 'b) -> 'a -> ('b, exn) result
(** [protect f] turns an [exception] throwing function into a [result] returning function. *)

module Pp : sig
  type 'a t = bool -> Format.formatter -> 'a -> unit

  val to_show : 'a t -> 'a -> string
  val of_show : ('a -> string) -> 'a t

  val cst0 : string -> Format.formatter -> unit
  val cst1 : 'a t -> string -> bool -> Format.formatter -> 'a -> unit
  val cst2 : 'a t -> 'b t -> string -> bool -> Format.formatter -> 'a -> 'b -> unit
  val cst3 : 'a t -> 'b t -> 'c t -> string -> bool -> Format.formatter -> 'a -> 'b -> 'c -> unit

  val pp_exn : exn t
  (** Format-based exception pretty printer *)

  val pp_unit : unit t
  val pp_bool : bool t
  val pp_int : int t
  val pp_int64 : int64 t
  val pp_float : float t
  val pp_char : char t
  val pp_string : string t
  val pp_bytes : bytes t
  val pp_option : 'a t -> 'a option t
  val pp_result : 'o t -> 'e t -> ('o, 'e) result t
  val pp_pair : 'a t -> 'b t -> ('a * 'b) t
  val pp_list : 'a t -> 'a list t
  val pp_seq : 'a t -> 'a Seq.t t
  val pp_array : 'a t -> 'a array t

  type pp_field
  val pp_field : string -> 'a t -> 'a -> pp_field
  val pp_record : pp_field list t
end

module Equal : sig
  type 'a t = 'a -> 'a -> bool

  val equal_exn : exn t
  (** equality function for comparing exceptions *)

  val equal_unit : unit t
  val equal_bool : bool t
  val equal_int : int t
  val equal_int64 : int64 t
  val equal_float : float t
  val equal_char : char t
  val equal_string : string t
  val equal_option : 'a t -> 'a option t
  val equal_result : 'o t -> 'e t -> ('o, 'e) result t
  val equal_list : 'a t -> 'a list t
  val equal_seq : 'a t -> 'a Seq.t t
  val equal_array : 'a t -> 'a array t
end
