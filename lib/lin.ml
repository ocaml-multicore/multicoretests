module Internal =
struct
  open QCheck
  include Util

  module Var =
  struct
    type t = int
    let next, reset =
      let counter = ref 0 in
      (fun () -> let old = !counter in
        incr counter; old),
      (fun () -> counter := 0)
    let show v = Format.sprintf "t%i" v
    let pp fmt v = Format.fprintf fmt "%s" (show v)
  end

  module Env =
  struct
    type t = Var.t list
    let gen_t_var env = Gen.oneofl env
    let valid_t_vars env v =
      if v = 0 then Iter.empty else
        Iter.of_list (List.rev (List.filter (fun v' -> v' < v) env))
  end

  module type CmdSpec = sig
    type t
    (** The type of the system under test *)

    type cmd
    (** The type of commands *)

    val show_cmd : cmd -> string
    (** [show_cmd c] returns a string representing the command [c]. *)

    val gen_cmd : Var.t Gen.t -> (Var.t option * cmd) Gen.t
    (** A command generator.
        It accepts a variable generator and generates a pair [(opt,cmd)] with the option indicating
        an storage index to store the [cmd]'s result. *)

    val shrink_cmd : Env.t -> cmd Shrink.t
    (** A command shrinker.
        It accepts the current environment as its argument.
        To a first approximation you can use [fun _env -> Shrink.nil]. *)

    val cmd_uses_var : Var.t -> cmd -> bool
    (** [cmd_uses_var v cmd] should return [true] iff the command [cmd] refers to the variable [v]. *)

    type res
    (** The command result type *)

    val show_res : res -> string
    (** [show_res r] returns a string representing the result [r]. *)

    val equal_res : res -> res -> bool

    val init : unit -> t
    (** Initialize the system under test. *)

    val cleanup : t -> unit
    (** Utility function to clean up [t] after each test instance,
        e.g., for closing sockets, files, or resetting global parameters *)

    val run : (Var.t option * cmd) -> t array -> res
    (** [run (opt,c) t] should interpret the command [c] over the various instances of the system under test [t array] (typically side-effecting).
        [opt] indicates the index to store the result. *)
end

  (** A functor to create test setups, for all backends (Domain, Thread and Effect).
      We use it below, but it can also be used independently *)
  module Make(Spec : CmdSpec)
  = struct

    (* plain interpreter of a cmd list *)
    let interp_plain sut cs = List.map (fun c -> (c, Spec.run c sut)) cs

    (* val gen_cmds : Env.t -> int -> (Env.t * (Var.t option * Spec.cmd) list) Gen.t *)
    let rec gen_cmds env fuel =
      Gen.(if fuel = 0
           then return (env,[])
           else
             Spec.gen_cmd (Env.gen_t_var env) >>= fun (opt,c) ->
               let env = match opt with None -> env | Some v -> v::env in
               gen_cmds env (fuel-1) >>= fun (env,cs) -> return (env,(opt,c)::cs))
    (** A fueled command list generator.
        Accepts an environment parameter [env] to enable [cmd] generation of multiple [t]s
        and returns the extended environment. *)

    let gen_cmds_size env size_gen = Gen.sized_size size_gen (gen_cmds env)

    (* Note that the result is built in reverse (ie should be
       _decreasing_) order *)
    let rec extract_env env cmds =
      match cmds with
      | []                  -> env
      | (Some i, _) :: cmds -> extract_env (i::env) cmds
      | (None,   _) :: cmds -> extract_env     env  cmds

    let cmds_use_var var cmds =
      List.exists (fun (_,c) -> Spec.cmd_uses_var var c) cmds

    let ctx_doesnt_use_var _var = false

    let rec shrink_cmd_list_spine cmds ctx_uses_var = match cmds with
      | [] -> Iter.empty
      | c::cs ->
        let open Iter in
        (match fst c with
         | None -> (* shrink tail first as fewer other vars should depend on it *)
           (map (fun cs -> c::cs) (shrink_cmd_list_spine cs ctx_uses_var)
            <+>
            return cs) (* not a t-assignment, so try without it *)
         | Some i -> (* shrink tail first as fewer other vars should depend on it *)
           (map (fun cs -> c::cs) (shrink_cmd_list_spine cs ctx_uses_var)
           <+>
           if cmds_use_var i cs || ctx_uses_var i
           then empty
           else return cs)) (* t_var not used, so try without it *)

    let rec shrink_individual_cmds env cmds = match cmds with
      | [] -> Iter.empty
      | (opt,cmd) :: cmds ->
        let env' = match opt with None -> env | Some i -> i::env in
        let open Iter in
        map (fun cmds -> (opt,cmd)::cmds) (shrink_individual_cmds env' cmds)
        <+>
        map (fun cmd -> (opt,cmd)::cmds) (Spec.shrink_cmd env cmd)

    let shrink_triple' (seq,p1,p2) =
      let open Iter in
      (* Shrinking heuristic:
         First reduce the cmd list sizes as much as possible, since the interleaving
         is most costly over long cmd lists. *)
      map (fun seq -> (seq,p1,p2)) (shrink_cmd_list_spine seq (fun var -> cmds_use_var var p1 || cmds_use_var var p2))
      <+>
      map (fun p1 -> (seq,p1,p2)) (shrink_cmd_list_spine p1 ctx_doesnt_use_var)
      <+>
      map (fun p2 -> (seq,p1,p2)) (shrink_cmd_list_spine p2 ctx_doesnt_use_var)
      <+> (* eta-expand the following two to lazily compute the match and @ until/if needed *)
      (fun yield -> (match p1 with [] -> Iter.empty | c1::c1s -> Iter.return (seq@[c1],c1s,p2)) yield)
      <+>
      (fun yield -> (match p2 with [] -> Iter.empty | c2::c2s -> Iter.return (seq@[c2],p1,c2s)) yield)
      <+>
      (* Secondly reduce the cmd data of individual list elements *)
      (fun yield ->
         let seq_env = extract_env [0] seq in (* only extract_env if needed *)
         (Iter.map (fun p1  -> (seq,p1,p2)) (shrink_individual_cmds seq_env p1)
          <+>
          Iter.map (fun p2  -> (seq,p1,p2)) (shrink_individual_cmds seq_env p2))
           yield)
      <+>
      Iter.map (fun seq -> (seq,p1,p2)) (shrink_individual_cmds [0] seq)

    let shrink_triple (size,t) =
      Iter.map (fun t -> (size,t)) (shrink_triple' t)


    let show_cmd (opt,c) = match opt with
      | None   -> Spec.show_cmd c
      | Some v -> Printf.sprintf "let %s = %s" (Var.show v) (Spec.show_cmd c)

    let init_cmd = "let t0 = init ()"
    let init_cmd_ret = init_cmd ^ " : ()"

    let arb_cmds_triple seq_len par_len =
      let gen_triple st =
        Var.reset ();
        let init_var = Var.next () in
        assert (init_var = 0);
        Gen.(int_range 2 (2*par_len) >>= fun dbl_plen ->
             let par_len1 = dbl_plen/2 in
             gen_cmds_size [init_var] (int_bound seq_len) >>= fun (env,seq_pref) ->
             gen_cmds_size env (return par_len1) >>= fun (_env1,par1) ->
             gen_cmds_size env (return (dbl_plen - par_len1)) >>= fun (_env2,par2) ->
             let array_size = Var.next () in
             return (array_size,(seq_pref,par1,par2))) st
      in
      make ~print:(fun (_,t) -> print_triple_vertical ~init_cmd show_cmd t) ~shrink:shrink_triple gen_triple

    let init_sut array_size =
      let sut = Spec.init () in
      Array.make array_size sut

    let cleanup sut seq_pref cmds1 cmds2 =
      let cleanup_opt (opt,_c) = match opt with
        | None -> ()
        | Some v -> Spec.cleanup sut.(v) in
      Spec.cleanup sut.(0); (* always present *)
      List.iter cleanup_opt seq_pref;
      List.iter cleanup_opt cmds1;
      List.iter cleanup_opt cmds2

    let cleanup_traces sut seq_pref cmds1 cmds2 =
      cleanup sut (List.map fst seq_pref) (List.map fst cmds1) (List.map fst cmds2)

    let rec check_seq_cons array_size pref cs1 cs2 seq_sut seq_trace = match pref with
      | (c,res)::pref' ->
        if Spec.equal_res res (Spec.run c seq_sut)
        then check_seq_cons array_size pref' cs1 cs2 seq_sut (c::seq_trace)
        else (cleanup_traces seq_sut pref cs1 cs2; false)
      (* Invariant: call Spec.cleanup immediately after mismatch  *)
      | [] -> match cs1,cs2 with
        | [],[] -> cleanup_traces seq_sut pref cs1 cs2; true
        | [],(c2,res2)::cs2' ->
          if Spec.equal_res res2 (Spec.run c2 seq_sut)
          then check_seq_cons array_size pref cs1 cs2' seq_sut (c2::seq_trace)
          else (cleanup_traces seq_sut pref cs1 cs2; false)
        | (c1,res1)::cs1',[] ->
          if Spec.equal_res res1 (Spec.run c1 seq_sut)
          then check_seq_cons array_size pref cs1' cs2 seq_sut (c1::seq_trace)
          else (cleanup_traces seq_sut pref cs1 cs2; false)
        | (c1,res1)::cs1',(c2,res2)::cs2' ->
          (if Spec.equal_res res1 (Spec.run c1 seq_sut)
           then check_seq_cons array_size pref cs1' cs2 seq_sut (c1::seq_trace)
           else (cleanup_traces seq_sut pref cs1 cs2; false))
          ||
          (* rerun to get seq_sut to same cmd branching point *)
          (let seq_sut' = init_sut array_size in
           let _ = interp_plain seq_sut' (List.rev seq_trace) in
           if Spec.equal_res res2 (Spec.run c2 seq_sut')
           then check_seq_cons array_size pref cs1 cs2' seq_sut' (c2::seq_trace)
           else (cleanup_traces seq_sut' pref cs1 cs2; false))

    (* Linearization test *)
    let lin_test ~rep_count ~retries ~count ~name ~lin_prop =
      let arb_cmd_triple = arb_cmds_triple 20 12 in
      Test.make ~count ~retries ~name
        arb_cmd_triple (repeat rep_count lin_prop)

    (* Negative linearization test *)
    let neg_lin_test ~rep_count ~retries ~count ~name ~lin_prop =
      let arb_cmd_triple = arb_cmds_triple 20 12 in
      Test.make_neg ~count ~retries ~name
        arb_cmd_triple (repeat rep_count lin_prop)
  end
end

(* Type-representing values *)

type constructible = |
type deconstructible = |

type combinable
type noncombinable

type (_,_,_,_) ty =
  | Gen : 'a QCheck.arbitrary * ('a -> string) -> ('a, constructible, 's, combinable) ty
  | Deconstr : ('a -> string) * ('a -> 'a -> bool) -> ('a, deconstructible, 's, combinable) ty
  | GenDeconstr : 'a QCheck.arbitrary * ('a -> string) * ('a -> 'a -> bool) -> ('a, 'c, 's, combinable) ty
  | State : ('s, constructible, 's, noncombinable) ty

let gen gen print = Gen (gen,print)
let deconstructible print eq = Deconstr (print,eq)
let gen_deconstructible gen print eq = GenDeconstr (gen,print,eq)

let qcheck_nat64_small = QCheck.(map Int64.of_int small_nat)

let print_char c   = Printf.sprintf "%C" c
let print_string s = Printf.sprintf "%S" s

let unit =           GenDeconstr (QCheck.unit,           QCheck.Print.unit, (=))
let bool =           GenDeconstr (QCheck.bool,           QCheck.Print.bool, (=))
let char =           GenDeconstr (QCheck.char,           print_char,        (=))
let char_printable = GenDeconstr (QCheck.printable_char, print_char,        (=))
let nat_small =      GenDeconstr (QCheck.small_nat,      QCheck.Print.int,  (=))
let int =            GenDeconstr (QCheck.int,            QCheck.Print.int,  (=))
let int_small =      GenDeconstr (QCheck.small_int,      QCheck.Print.int,  (=))
let int_pos =        GenDeconstr (QCheck.pos_int,        QCheck.Print.int,  (=))
let int_bound b =    GenDeconstr (QCheck.int_bound b,    QCheck.Print.int,  (=))
let int32 =          GenDeconstr (QCheck.int32,          Int32.to_string,   Int32.equal)
let int64 =          GenDeconstr (QCheck.int64,          Int64.to_string,   Int64.equal)
let nat64_small =    GenDeconstr (qcheck_nat64_small,    Int64.to_string,   Int64.equal)
let float =          GenDeconstr (QCheck.float,          Float.to_string,   Float.equal)
let string =         GenDeconstr (QCheck.string,         print_string,      String.equal)
let string_small =   GenDeconstr (QCheck.small_string,   print_string,      String.equal)
let string_small_printable = GenDeconstr (QCheck.small_printable_string,   print_string,      String.equal)

let option : type a c s. ?ratio:float -> (a, c, s, combinable) ty -> (a option, c, s, combinable) ty =
  fun ?ratio ty ->
  match ty with
  | Gen (arb, print) -> Gen (QCheck.option ?ratio arb, QCheck.Print.option print)
  | GenDeconstr (arb, print, eq) -> GenDeconstr (QCheck.option ?ratio arb, QCheck.Print.option print, Option.equal eq)
  | Deconstr (print, eq) -> Deconstr (QCheck.Print.option print, Option.equal eq)
let opt = option

let list : type a c s. (a, c, s, combinable) ty -> (a list, c, s, combinable) ty =
  fun ty -> match ty with
    | Gen (arb, print) -> Gen (QCheck.list arb, QCheck.Print.list print)
    | GenDeconstr (arb, print, eq) -> GenDeconstr (QCheck.list arb, QCheck.Print.list print, List.equal eq)
    | Deconstr (print, eq) -> Deconstr (QCheck.Print.list print, List.equal eq)

let array : type a c s. (a, c, s, combinable) ty -> (a array, c, s, combinable) ty =
  fun ty -> match ty with
    | Gen (arb, print) -> Gen (QCheck.array arb, QCheck.Print.array print)
    | GenDeconstr (arb, print, eq) -> GenDeconstr (QCheck.array arb, QCheck.Print.array print, Array.for_all2 eq)
    | Deconstr (print, eq) -> Deconstr (QCheck.Print.array print, Array.for_all2 eq)

let bounded_array : type a c s. int -> (a, c, s, combinable) ty -> (a array, c, s, combinable) ty =
  fun maxsize ty ->
  let array = QCheck.array_of_size (QCheck.Gen.int_bound maxsize) in
  match ty with
  | Gen (arb, print) -> Gen (array arb, QCheck.Print.array print)
  | GenDeconstr (arb, print, eq) -> GenDeconstr (array arb, QCheck.Print.array print, Array.for_all2 eq)
  | Deconstr (print, eq) -> Deconstr (QCheck.Print.array print, Array.for_all2 eq)

let print_seq pp s =
  let b = Buffer.create 25 in
  Buffer.add_char b '<';
  Seq.iteri (fun i x ->
      if i > 0 then Buffer.add_string b "; ";
      Buffer.add_string b (pp x))
    s;
  Buffer.add_char b '>';
  Buffer.contents b

let arb_seq a =
  let open QCheck in
  let print = match a.print with None -> None | Some ap -> Some (print_seq ap) in
  let shrink s = Iter.map List.to_seq (Shrink.list ?shrink:a.shrink (List.of_seq s)) in
  let gen = Gen.map List.to_seq (Gen.list a.gen) in
  QCheck.make ?print ~shrink gen

let seq : type a c s. (a, c, s, combinable) ty -> (a Seq.t, c, s, combinable) ty =
  fun ty -> match ty with
    | Gen (arb, print) -> Gen (arb_seq arb, print_seq print)
    | GenDeconstr (arb, print, eq) -> GenDeconstr (arb_seq arb, print_seq print, Seq.equal eq)
    | Deconstr (print, eq) -> Deconstr (print_seq print, Seq.equal eq)

let state = State
let t = state

let print_result print_ok print_err = function
  | Ok x    -> Printf.sprintf "Ok (%s)" (print_ok x)
  | Error y -> Printf.sprintf "Error (%s)" (print_err y)

let or_exn ty = match ty with
  | GenDeconstr (_, print, eq) ->
    Deconstr (print_result print Printexc.to_string, Result.equal ~ok:eq ~error:(=))
  | Deconstr (print, eq) ->
    Deconstr (print_result print Printexc.to_string, Result.equal ~ok:eq ~error:(=))

let print : type a c s comb. (a, c, s, comb) ty -> a -> string = fun ty value ->
  match ty with
  | Gen (_,print) -> print value
  | Deconstr (print,_) -> print value
  | GenDeconstr (_,print,_) -> print value
  | State -> "t"

let equal : type a s c. (a, deconstructible, s, c) ty -> a -> a -> bool = fun ty ->
  match ty with
  | Deconstr (_,equal) -> equal
  | GenDeconstr (_,_,equal) -> equal

module Fun = struct
  (* Function type, number of arguments (unary encoding), state type *)
  type (_,_,_) fn =
    | Ret :        ('a, deconstructible, 's, combinable) ty -> ('a, 'a, 's) fn
    | Ret_or_exc : ('a, deconstructible, 's, combinable) ty -> ('a, ('a,exn) result, 's) fn
    | Ret_ignore : ('a, _, 's, _) ty -> ('a, unit, 's) fn
    | Ret_ignore_or_exc : ('a, _, 's, _) ty -> ('a, (unit,exn) result, 's) fn
    | Fn : ('a, constructible, 's, _) ty * ('b, 'r, 's) fn -> ('a -> 'b, 'r, 's) fn
end

let returning a = Fun.Ret a
let returning_or_exc a = Fun.Ret_or_exc a
let returning_ a = Fun.Ret_ignore a
let returning_or_exc_ a = Fun.Ret_ignore_or_exc a
let (@->) a b = Fun.Fn (a,b)

type _ elem =
  | Elem : string * ('ftyp, 'r, 's) Fun.fn * 'ftyp -> 's elem

type 's api = (int * 's elem) list

let val_ name value fntyp =
  (1, Elem (name, fntyp, value))

let val_freq freq name value fntyp =
  (freq, Elem (name, fntyp, value))

module type Spec = sig
  type t
  val init : unit -> t
  val cleanup : t -> unit
  val api : (int * t elem) list
end

module MakeCmd (ApiSpec : Spec) : Internal.CmdSpec = struct

  type t = ApiSpec.t

  let init = ApiSpec.init
  let cleanup = ApiSpec.cleanup

  (* Typed argument list and return type descriptor *)
  module Args = struct
    type (_,_) args =
      | Ret : ('a, deconstructible, t, _) ty -> ('a,'a) args
      | Ret_or_exc : ('a, deconstructible, t, _) ty -> ('a, ('a,exn) result) args
      | Ret_ignore : ('a, _, t, _) ty -> ('a, unit) args
      | Ret_ignore_or_exc : ('a, _, t, _) ty -> ('a, (unit,exn) result) args
      | Fn : 'a * ('b,'r) args -> ('a -> 'b, 'r) args
      | FnState : Internal.Var.t * ('b,'r) args -> (t -> 'b, 'r) args
  end

  (* Operation name, typed argument list, return type descriptor, printer, shrinker, function *)
  type cmd =
      Cmd :
        string *
        ('ftyp, 'r) Args.args *
        ('r, deconstructible, t, _) ty *
        (('ftyp, 'r) Args.args -> string) *
        (('ftyp, 'r) Args.args QCheck.Shrink.t) *
        'ftyp
        -> cmd

  type res =
      Res : ('a, deconstructible, t, _) ty * 'a -> res

  (* Function to generate typed list of arguments from a function description.
     The printer can be generated independently. *)
  let rec gen_args_of_desc
    : type a r. (a, r, t) Fun.fn -> (Internal.Var.t QCheck.Gen.t) -> ((a, r) Args.args) QCheck.Gen.t =
    fun fdesc gen_var ->
    let open QCheck.Gen in
    match fdesc with
    | Fun.Ret ty -> return @@ Args.Ret ty
    | Fun.Ret_or_exc ty -> return @@ Args.Ret_or_exc ty
    | Fun.Ret_ignore_or_exc ty -> return @@ Args.Ret_ignore_or_exc ty
    | Fun.Ret_ignore ty -> return @@ Args.Ret_ignore ty
    | Fun.(Fn (State, fdesc_rem)) ->
        map2 (fun state_arg args_rem -> Args.FnState (state_arg, args_rem))
          gen_var (gen_args_of_desc fdesc_rem gen_var)
     (* let* state_arg = gen_var in
        let* args_rem = gen_args_of_desc fdesc_rem gen_var in
        return @@ Args.FnState (state_arg, args_rem) *)
    | Fun.(Fn ((Gen (arg_arb,_) | GenDeconstr (arg_arb, _, _)), fdesc_rem)) ->
        map2 (fun arg args_rem -> Args.Fn (arg, args_rem))
          arg_arb.gen (gen_args_of_desc fdesc_rem gen_var)
     (* let* arg = arg_arb.gen in
        let* args_rem = gen_args_of_desc fdesc_rem gen_var in
        return @@ Args.Fn (arg, args_rem) *)

  let rec ret_type
    : type a r. (a,r,t) Fun.fn -> Internal.Var.t option * (r, deconstructible, t, _) ty
    = fun fdesc ->
      match fdesc with
      | Fun.Ret ty -> None, ty
      | Fun.Ret_or_exc ty -> None, or_exn ty
      | Fun.Ret_ignore ty -> (match ty with
          | State -> Some (Internal.Var.next ()), unit
          | _     -> None, unit)
      | Fun.Ret_ignore_or_exc ty -> (match ty with
          | State -> Some (Internal.Var.next ()), or_exn unit
          | _     -> None, or_exn unit)
      | Fun.Fn (_, fdesc_rem) -> ret_type fdesc_rem

  let rec show_args : type a r. (a,r,t) Fun.fn -> (a,r) Args.args -> string list = fun fdesc args ->
    match fdesc,args with
    | _, Args.(Ret _ | Ret_or_exc _ | Ret_ignore _ | Ret_ignore_or_exc _) -> []
    | Fun.(Fn (State, fdesc_rem)), Args.(FnState (index,args_rem)) ->
        (Internal.Var.show index)::(show_args fdesc_rem args_rem)
    | Fun.(Fn ((GenDeconstr _ | Gen _ as ty), fdesc_rem)), Args.(Fn (value, args_rem)) ->
      (print ty value)::show_args fdesc_rem args_rem
    | Fun.(Fn (State, _)), Args.(Fn _)
    | Fun.(Fn ((Gen _ | GenDeconstr _), _)), Args.(FnState _)  ->
      assert false
    | Fun.(Ret _ | Ret_or_exc _ | Ret_ignore _ | Ret_ignore_or_exc _), Args.(Fn _ | FnState _) ->
      assert false

  let gen_printer : type a r. string -> (a,r,t) Fun.fn -> (a,r) Args.args -> string = fun name fdesc args ->
    name ^ " " ^ (String.concat " " (show_args fdesc args))

  (* Extracts a QCheck shrinker for argument lists *)
  let rec gen_shrinker_of_desc
    : type a r. (a, r, t) Fun.fn -> ((a, r) Args.args) QCheck.Shrink.t =
    fun fdesc ->
    let open QCheck in
    match fdesc with
    | Fun.Ret _ty -> Shrink.nil
    | Fun.Ret_or_exc _ty -> Shrink.nil
    | Fun.Ret_ignore_or_exc _ty -> Shrink.nil
    | Fun.Ret_ignore _ty -> Shrink.nil
    | Fun.(Fn (State, fdesc_rem)) ->
        (function (Args.FnState (index,args)) ->
          Iter.map (fun args -> Args.FnState (index,args)) (gen_shrinker_of_desc fdesc_rem args)
                | _ -> failwith "FnState: should not happen")
    | Fun.(Fn ((Gen (arg_arb,_) | GenDeconstr (arg_arb, _, _)), fdesc_rem)) ->
      (match arg_arb.shrink with
       | None ->
         (function (Args.Fn (a,args)) ->
            Iter.map (fun args -> Args.Fn (a,args)) (gen_shrinker_of_desc fdesc_rem args)
                 | _ -> failwith "Fn/None: should not happen")
       | Some shrk ->
         Iter.(function (Args.Fn (a,args)) ->
             (map (fun a -> Args.Fn (a,args)) (shrk a))
             <+>
             (map (fun args -> Args.Fn (a,args)) (gen_shrinker_of_desc fdesc_rem args))
                      | _ -> failwith "Fn/Some: should not happen"))

  let api gen_t_var =
    List.map (fun (wgt, Elem (name, fdesc, f)) ->
        let print = gen_printer name fdesc in
        let shrink = gen_shrinker_of_desc fdesc in
        let open QCheck.Gen in
        (wgt, gen_args_of_desc fdesc gen_t_var >>= fun args ->
         let opt_var, rty = ret_type fdesc in
         return (opt_var, Cmd (name, args, rty, print, shrink, f)))) ApiSpec.api

  let gen_cmd gen_t_var : ('a option * cmd) QCheck.Gen.t = QCheck.Gen.frequency (api gen_t_var)

  let show_cmd (Cmd (_,args,_,print,_,_)) = print args

  let rec args_use_var
    : type a r. Internal.Var.t -> (a, r) Args.args -> bool =
    fun var args ->
    match args with
    | FnState (i, args') -> i=var || args_use_var var args'
    | Fn (_, args')      -> args_use_var var args'
    | _                  -> false

  let cmd_uses_var var (Cmd (_,args,_,_,_,_)) = args_use_var var args

  let rec shrink_t_vars
    : type a r. Internal.Env.t -> (a, r) Args.args -> (a, r) Args.args QCheck.Iter.t =
    fun env args ->
    match args with
    | FnState (i,args') ->
      QCheck.Iter.(map (fun i -> Args.FnState (i,args')) (Internal.Env.valid_t_vars env i)
                   <+>
                   map (fun args' -> Args.FnState (i, args')) (shrink_t_vars env args'))
    | Fn (a,args') -> QCheck.Iter.map (fun args' -> Args.Fn (a, args')) (shrink_t_vars env args')
    | _            -> QCheck.Iter.empty

  let shrink_cmd env (Cmd (name,args,rty,print,shrink,f)) =
    QCheck.Iter.( (* reduce t-vars first as it can trigger removal of other cmds *)
      map (fun args -> Cmd (name,args,rty,print,shrink,f)) (shrink_t_vars env args)
      <+> (* only secondly reduce char/int/... arguments *)
      map (fun args -> Cmd (name,args,rty,print,shrink,f)) (shrink args)
    )

  (* Unsafe if called on two [res] whose internal values are of different
     types. *)
  let equal_res (Res (deconstr, v1)) (Res (_, v2)) =
    match deconstr with
    | Deconstr (_, eq) -> eq v1 (Obj.magic v2)
    | GenDeconstr (_, _, eq) -> eq v1 (Obj.magic v2)

  let show_res (Res (deconstr, value)) =
    match deconstr with
    | Deconstr (print, _) -> print value
    | GenDeconstr (_, print, _) -> print value

  let rec apply_f
    : type a r. a -> (a, r) Args.args -> t array -> Internal.Var.t option -> r = fun f args state opt_target ->
    match args with
    | Ret _ ->
      f
    | Ret_or_exc _ ->
      (* A constant value in the API cannot raise an exception *)
      raise (Invalid_argument "apply_f")
    | Ret_ignore _ ->
      ()
    | Ret_ignore_or_exc _ ->
      (* A constant value in the API cannot raise an exception *)
      raise (Invalid_argument "apply_f")
    | FnState (index, Ret _) ->
        f state.(index)
    | FnState (index, Ret_or_exc _) ->
        begin
          try Ok (f state.(index))
          with e -> Error e
        end
    | FnState (index, Ret_ignore ty) ->
      (match ty,opt_target with
       | State, Some tgt -> state.(tgt) <- (f state.(index))
       | _               -> ignore (f state.(index)))
    | FnState (index, Ret_ignore_or_exc ty) ->
      (match ty,opt_target with
       | State, Some tgt ->
         begin
           try Ok (state.(tgt) <- f state.(index))
           with e -> Error e
         end
       | _ ->
         begin
           try Ok (ignore @@ f state.(index))
           with e -> Error e
         end)
    | FnState (index, rem) ->
        apply_f (f state.(index)) rem state opt_target
    | Fn (arg, Ret _) ->
      f arg
    | Fn (arg, Ret_or_exc _) ->
      begin
        try Ok (f arg)
        with e -> Error e
      end
    | Fn (arg, Ret_ignore _) ->
      ignore @@ f arg
    | Fn (arg, Ret_ignore_or_exc _) ->
      begin
        try Ok (ignore @@ f arg)
        with e -> Error e
      end
    | Fn (arg, rem) ->
      apply_f (f arg) rem state opt_target

  let run (opt_target,cmd) state =
    let Cmd (_, args, rty, _, _, f) = cmd in
    Res (rty, apply_f f args state opt_target)
end
