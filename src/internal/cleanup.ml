open QCheck

(** This is a variant of refs to test for double cleanup *)

module RConf =
struct
  exception Already_cleaned
  type status = Inited | Cleaned

  type cmd =
    | Get
    | Set of int
    | Add of int [@@deriving show { with_path = false }]

  type t = (status ref) * (int ref)

  let gen_cmd =
    let int_gen = Gen.nat in
      (Gen.oneof
         [Gen.return Get;
	  Gen.map (fun i -> Set i) int_gen;
	  Gen.map (fun i -> Add i) int_gen;
         ])

  let shrink_cmd = Shrink.nil

  let init () = (ref Inited, ref 0)

  let cleanup (status,_) =
    if !status = Cleaned
    then raise Already_cleaned
    else status := Cleaned

  type res = RGet of int | RSet | RAdd [@@deriving show { with_path = false }, eq]

  let run c (_,r) = match c with
    | Get   -> RGet (!r)
    | Set i -> (r:=i; RSet)
    | Add i -> (let old = !r in r := i + old; RAdd) (* buggy: not atomic *)
end

module RT = Lin_domain.Make_internal(RConf)
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
