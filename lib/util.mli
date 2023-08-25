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
  (** Pretty-printing combinators that generate valid OCaml syntax for common
      types along with combinators for user-defined types *)

  type 'a t = bool -> Format.formatter -> 'a -> unit
  (** The type of pretty-printers to valid OCaml syntax.
      The [bool] argument asks the printer to wrap its output inside parentheses
      if it produces a non-atomic expression. *)

  val to_show : 'a t -> 'a -> string
  (** [to_show pp] converts a pretty-printer to a simple ['a -> string] function
      that generate everything on one line. If the environment variable
      [MCTUTILS_TRUNCATE] is set to a length, it will truncate the resulting
      string if it exceeds that length. *)

  val of_show : ('a -> string) -> 'a t
  (** [of_show show] uses a simple ['a -> string] function as a pretty-printer.
      Unfortunately, it will wrap the resulting string with parentheses in more
      cases than strictly necessary.  *)

  val cst0 : string -> Format.formatter -> unit
  (** [cst0 name fmt] pretty-prints a constructor [name] with no argument. *)

  val cst1 : 'a t -> string -> bool -> Format.formatter -> 'a -> unit
  (** [cst1 pp name par v fmt] pretty-prints a constructor [name] with one
      parameter, using [pp] to pretty-print its argument [v], wrapping itself
      into parentheses when [par]. *)

  val cst2 : 'a t -> 'b t -> string -> bool -> Format.formatter -> 'a -> 'b -> unit
  (** [cst2 pp1 pp2 name par v1 v2 fmt] pretty-prints a constructor [name] with
      two parameters, using [pp]i to pretty-print its argument [v]i, wrapping
      itself into parentheses when [par]. *)

  val cst3 : 'a t -> 'b t -> 'c t -> string -> bool -> Format.formatter -> 'a -> 'b -> 'c -> unit
  (** [cst3 pp1 pp2 pp3 name par v1 v2 v3 fmt] pretty-prints a constructor
      [name] with three parameters, using [pp]i to pretty-print its argument
      [v]i, wrapping itself into parentheses when [par]. *)

  val pp_exn : exn t
  (** Pretty-printer for exceptions reusing the standard {!Printexc.to_string}.
      The exception message will be wrapped conservatively (ie too often) in
      parentheses. *)

  val pp_unit : unit t
  (** Pretty-printer for type [unit] *)

  val pp_bool : bool t
  (** Pretty-printer for type [bool] *)

  val pp_int : int t
  (** Pretty-printer for type [int] *)

  val pp_int32 : int32 t
  (** Pretty-printer for type [int32] *)

  val pp_int64 : int64 t
  (** Pretty-printer for type [int64] *)

  val pp_float : float t
  (** Pretty-printer for type [float] *)

  val pp_char : char t
  (** Pretty-printer for type [char] *)

  val pp_string : string t
  (** Pretty-printer for type [string] *)

  val pp_bytes : bytes t
  (** Pretty-printer for type [bytes] *)

  val pp_option : 'a t -> 'a option t
  (** [pp_option pp] pretty-prints a value of type ['a option] using [pp] to
      pretty-print values of type ['a]. *)

  val pp_result : 'o t -> 'e t -> ('o, 'e) result t
  (** [pp_result pp_ok pp_error] pretty-prints a value of type [('o, 'e) result]
      using [pp_ok] to pretty-print values of type ['o] and [pp_error] for
      values of type ['e]. *)

  type pp_tuple_item
  (** The abstract type for the pretty-printer of a tuple item *)

  val pp_tuple_item : 'a t -> 'a -> pp_tuple_item
  (** [pp_tuple_item pp v] builds a pretty-printer for a tuple item using [pp]
      to pretty-print its value [v]. *)

  val pp_tuple : pp_tuple_item list t
  (** [pp_tuple] pretty-prints a tuple taken as a list of [pp_tuple_item]s. *)

  val pp_pair : 'a t -> 'b t -> ('a * 'b) t
  (** [pp_pair pp_a pp_b] pretty-prints a value of type ['a * 'b] using [pp_a]
      to pretty-print values of type ['a] and [pp_b] for values of type ['b]. *)

  val pp_tuple2 : 'a t -> 'b t -> ('a * 'b) t
  (** [pp_tuple2] pretty-prints pairs, synonym for [pp_pair]. *)

  val pp_tuple3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  (** [pp_tuple3] pretty-prints triples. *)

  val pp_tuple4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  (** [pp_tuple4] pretty-prints tuples of 4 elements. *)

  val pp_tuple5 :
    'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t
  (** [pp_tuple5] pretty-prints tuples of 5 elements. *)

  val pp_tuple6 :
    'a t ->
    'b t ->
    'c t ->
    'd t ->
    'e t ->
    'f t ->
    ('a * 'b * 'c * 'd * 'e * 'f) t
  (** [pp_tuple6] pretty-prints tuples of 6 elements. *)

  val pp_tuple7 :
    'a t ->
    'b t ->
    'c t ->
    'd t ->
    'e t ->
    'f t ->
    'g t ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g) t
  (** [pp_tuple7] pretty-prints tuples of 7 elements. *)

  val pp_tuple8 :
    'a t ->
    'b t ->
    'c t ->
    'd t ->
    'e t ->
    'f t ->
    'g t ->
    'h t ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) t
  (** [pp_tuple8] pretty-prints tuples of 8 elements. *)

  val pp_tuple9 :
    'a t ->
    'b t ->
    'c t ->
    'd t ->
    'e t ->
    'f t ->
    'g t ->
    'h t ->
    'i t ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) t
  (** [pp_tuple9] pretty-prints tuples of 9 elements. *)

  val pp_tuple10 :
    'a t ->
    'b t ->
    'c t ->
    'd t ->
    'e t ->
    'f t ->
    'g t ->
    'h t ->
    'i t ->
    'j t ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j) t
  (** [pp_tuple10] pretty-prints tuples of 10 elements. *)

  val pp_list : 'a t -> 'a list t
  (** [pp_list pp] pretty-prints a list using [pp] to pretty-print its elements. *)

  val pp_seq : 'a t -> 'a Seq.t t
  (** [pp_seq pp] pretty-prints a sequence using [pp] to pretty-print its elements. *)

  val pp_array : 'a t -> 'a array t
  (** [pp_array pp] pretty-prints an array using [pp] to pretty-print its elements. *)

  type pp_field
  (** The abstract type for the pretty-printer of a record field *)

  val pp_field : string -> 'a t -> 'a -> pp_field
  (** [pp_field name pp v] builds a pretty-printer for a record field of given
      [name] using [pp] to pretty-print its content value [v]. *)

  val pp_record : pp_field list t
  (** [pp_record flds] pretty-prints a record using the list of pretty-printers
      of its fields. *)
end

module Equal : sig
  (** Equality combinators for common types *)

  type 'a t = 'a -> 'a -> bool
  (** The usual type for equality functions *)

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
