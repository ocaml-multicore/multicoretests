open QCheck

(** This is a variant of refs to test for double cleanup *)

module RConf =
struct
  exception Already_cleaned
  type status = Inited | Cleaned

  type cmd =
    | Get of Lin.Var.t
    | Set of Lin.Var.t * int
    | Add of Lin.Var.t * int [@@deriving show { with_path = false }]

  type t = (status ref) * (int ref)

  let gen_cmd gen_var =
    let int_gen = Gen.nat in
      (Gen.oneof
         [Gen.map  (fun t ->   None, Get t) gen_var;
	  Gen.map2 (fun t i -> None, Set (t,i)) gen_var int_gen;
	  Gen.map2 (fun t i -> None, Add (t,i)) gen_var int_gen;
         ])

  let shrink_cmd = Shrink.nil

  let fix_cmd env = function
    | Get i     -> Iter.map (fun i -> Get i    ) (Lin.Env.valid_t_vars env i)
    | Set (i,x) -> Iter.map (fun i -> Set (i,x)) (Lin.Env.valid_t_vars env i)
    | Add (i,x) -> Iter.map (fun i -> Add (i,x)) (Lin.Env.valid_t_vars env i)

  let init () = (ref Inited, ref 0)

  let cleanup (status,_) =
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

module RT = Lin.Make(RConf)
;;
Util.set_ci_printing ()
;;
Test.check_exn
  (let seq_len,par_len = 20,15 in
   Test.make ~count:1000 ~name:("avoid double cleanup test")
     (RT.arb_cmds_par seq_len par_len)
     (fun input ->
        try
          ignore (RT.lin_prop_domain input);
          true
        with
        | RConf.Already_cleaned -> failwith "Already cleaned"
        | _ -> true
     ))
