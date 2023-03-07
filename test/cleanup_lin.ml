open QCheck

(** This is a variant of refs to test for exactly one cleanup per init
    (no double cleanup, no missing cleanup) *)

let cleanup_counter = Atomic.make 0

module RConf =
struct
  open Lin.Internal [@@alert "-internal"]

  exception Already_cleaned
  type status = Inited | Cleaned

  type cmd =
    | Get of Var.t
    | Set of Var.t * int
    | Add of Var.t * int [@@deriving show { with_path = false }]

  type t = (status ref) * (int ref)

  let gen_cmd gen_var =
    let int_gen = Gen.nat in
      (Gen.oneof
         [Gen.map  (fun t ->   None, Get t) gen_var;
	  Gen.map2 (fun t i -> None, Set (t,i)) gen_var int_gen;
	  Gen.map2 (fun t i -> None, Add (t,i)) gen_var int_gen;
         ])

  let shrink_cmd _env = Shrink.nil

  let cmd_uses_var v = function
    | Get i
    | Set (i,_)
    | Add (i,_) -> i=v

  let init () =
    Atomic.incr cleanup_counter ;
    (ref Inited, ref 0)

  let cleanup (status,_) =
    Atomic.decr cleanup_counter ;
    if !status = Cleaned
    then raise Already_cleaned
    else status := Cleaned

  type res = RGet of int | RSet | RAdd [@@deriving show { with_path = false }, eq]

  let run c s = match c with
    | None, Get t     -> RGet (!(snd s.(t)))
    | None, Set (t,i) -> ((snd s.(t)):=i; RSet)
    | None, Add (t,i) -> (let old = !(snd (s.(t))) in snd (s.(t)) := i + old; RAdd) (* buggy: not atomic *)
    | _, _ -> failwith (Printf.sprintf "unexpected command: %s" (show_cmd (snd c)))
end

module RT = Lin_domain.Make_internal(RConf) [@alert "-internal"]
;;
Test.check_exn
  (let seq_len,par_len = 20,15 in
   Test.make ~count:1000 ~name:("exactly one-cleanup test")
     (RT.arb_cmds_triple seq_len par_len)
     (fun input ->
        try
          ignore (RT.lin_prop input);
          Atomic.get cleanup_counter = 0
        with
        | RConf.Already_cleaned -> failwith "Already cleaned"
        | _ -> Atomic.get cleanup_counter = 0
     ))
