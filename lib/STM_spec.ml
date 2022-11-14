open QCheck

type 'a ty = ..

type _ ty +=
  | Unit : unit ty
  | Bool : bool ty
  | Char : char ty
  | Int : int ty
  | Int32 : int32 ty
  | Int64 : int64 ty
  | Float : float ty
  | String : string ty
  | Bytes : bytes ty
  | Exn : exn ty
  | Option : 'a ty -> 'a option ty
  | Result : 'a ty * 'b ty -> ('a, 'b) result ty
  | List : 'a ty -> 'a list ty
  | Array : 'a ty -> 'a array ty
  | Seq : 'a ty -> 'a Seq.t ty

type 'a ty_show = 'a ty * ('a -> string)

let unit = (Unit, fun () -> "()")
let bool = (Bool, string_of_bool)
let char = (Char, fun c -> Printf.sprintf "%C" c)
let int = (Int, string_of_int)
let int32 = (Int32, Int32.to_string)
let int64 = (Int64, Int64.to_string)
let float = (Float, Float.to_string)
let string = (String, QCheck.Print.string)
let bytes = (Bytes, fun b -> QCheck.Print.string (Bytes.to_string b))
let option spec =
  let (ty,show) = spec in
  (Option ty, QCheck.Print.option show)
let exn = (Exn, Printexc.to_string)

let show_result show_ok show_err = function
  | Ok x    -> Printf.sprintf "Ok (%s)" (show_ok x)
  | Error y -> Printf.sprintf "Error (%s)" (show_err y)

let result spec_ok spec_err =
  let (ty_ok, show_ok) = spec_ok in
  let (ty_err, show_err) = spec_err in
  (Result (ty_ok, ty_err), show_result show_ok show_err)
let list spec =
  let (ty,show) = spec in
  (List ty, QCheck.Print.list show)
let array spec =
  let (ty,show) = spec in
  (Array ty, QCheck.Print.array show)
let seq spec =
  let (ty,show) = spec in
  (Seq ty, fun s -> QCheck.Print.list show (List.of_seq s))



type res =
  Res : 'a ty_show * 'a -> res

let show_res (Res ((_,show), v)) = show v

(** The specification of a state machine. *)
module type Spec =
sig
  type cmd
  (** The type of commands *)

  type state
  (** The type of the model's state *)

  type sut
  (** The type of the system under test *)

  val arb_cmd : state -> cmd arbitrary
  (** A command generator. Accepts a state parameter to enable state-dependent [cmd] generation. *)

  val init_state : state
  (** The model's initial state. *)

  val show_cmd : cmd -> string
  (** [show_cmd c] returns a string representing the command [c]. *)
 
  val next_state : cmd -> state -> state
  (** Move the internal state machine to the next state. *)

  val init_sut : unit -> sut
  (** Initialize the system under test. *)

  val cleanup : sut -> unit
  (** Utility function to clean up the [sut] after each test instance,
      e.g., for closing sockets, files, or resetting global parameters*)

  val precond : cmd -> state -> bool
  (** [precond c s] expresses preconditions for command [c].
      This is useful, e.g., to prevent the shrinker from breaking invariants when minimizing
      counterexamples. *)

 val run : cmd -> sut -> res
  (** [run c i] should interpret the command [c] over the system under test (typically side-effecting). *)

  val postcond : cmd -> state -> res -> bool
  (** [postcond c s res] checks whether [res] arising from interpreting the
      command [c] over the system under test with [run] agrees with the
      model's result.
      Note: [s] is in this case the model's state prior to command execution. *)
end

module AddGC(Spec : Spec) : Spec = struct
  type cmd =
    | GC_minor
    | UserCmd of Spec.cmd

  type state = Spec.state
  type sut   = Spec.sut

  let init_state  = Spec.init_state
  let init_sut () = Spec.init_sut ()
  let cleanup sut = Spec.cleanup sut

  let show_cmd c = match c with
    | GC_minor -> "<GC.minor>"
    | UserCmd c -> Spec.show_cmd c

  let gen_cmd s =
    (Gen.frequency
       [(1,Gen.return GC_minor);
        (5,Gen.map (fun c -> UserCmd c) (Spec.arb_cmd s).gen)])

  let shrink_cmd s c = match c with
    | GC_minor  -> Iter.empty
    | UserCmd c ->
       match (Spec.arb_cmd s).shrink with
       | None     -> Iter.empty (* no shrinker provided *)
       | Some shk -> Iter.map (fun c' -> UserCmd c') (shk c)

  let arb_cmd s = make ~print:show_cmd ~shrink:(shrink_cmd s) (gen_cmd s)

  let next_state c s = match c with
    | GC_minor  -> s
    | UserCmd c -> Spec.next_state c s

  let precond c s = match c with
    | GC_minor  -> true
    | UserCmd c -> Spec.precond c s

  let run c s = match c with
    | GC_minor  -> (Gc.minor (); Res (unit, ()))
    | UserCmd c -> Spec.run c s

  let postcond c s r = match c,r with
    | GC_minor,  Res ((Unit,_),_) -> true
    | UserCmd c, r -> Spec.postcond c s r
    | _,_ -> false
end
