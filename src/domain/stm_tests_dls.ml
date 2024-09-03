open Domain
open QCheck
open STM

(** parallel STM tests of Domain.DLS *)

module DLSConf =
struct
  let length = 8

  type index = int
  type cmd =
  (*| Get of index*)
    | Set of index * int

  let pp_cmd par fmt x =
    let open Util.Pp in
    match x with
  (*| Get i -> cst1 pp_int "Get" par fmt i*)
    | Set (i,x) -> cst2 pp_int pp_int "Set" par fmt i x

  let show_cmd = Util.Pp.to_show pp_cmd

  type state = int list
  type sut   = int Domain.DLS.key list

  let arb_cmd _s =
    let index = Gen.int_bound (length-1) in
    let int_gen = Gen.small_nat in
    QCheck.make ~print:show_cmd
      Gen.(oneof
             [ (*map (fun i -> Get i) index;*)
               map2 (fun i x -> Set (i,x)) index int_gen;
             ])

  let init_state  = List.init length (fun i -> i)

  let next_state n s = match n with
  (*| Get _     -> s*)
    | Set (i,n) -> List.mapi (fun j x -> if i=j then n else x) s

  let init_sut () = List.init length (fun i -> DLS.new_key (fun () -> i))

  let cleanup _   = ()

  let precond n _s = match n with
    | _ -> true

  let run n t = match n with
  (*| Get i     -> Res (STM.int, Domain.DLS.get (List.nth t i))*)
    | Set (i,x) -> Res (unit, Domain.DLS.set (List.nth t i) x)

  let postcond n (_s:int list) res = match n, res with
  (*| Get i, Res ((Int,_), r) -> (List.nth s i) = r*)
    | Set _, Res ((Unit,_), ()) -> true
    | _, _ -> false
end

module DLS_STM_seq = STM_sequential.Make(DLSConf)
module DLS_STM_dom = STM_domain.Make(DLSConf)

(* Run seq. property in a child domain to have a clean DLS for each iteration *)
let agree_prop cs = match Domain.spawn (fun () -> Util.protect DLS_STM_seq.agree_prop cs) |> Domain.join with
  | Ok r -> r
  | Error e -> raise e

(*let gen_cmds_size gen s size_gen = Gen.sized_size size_gen (gen_cmds gen s)*)

let _exp_dist_gen =
  let mean = 10. in
  let skew = 0.75 in (* to avoid too many empty cmd lists *)
  let unit_gen = Gen.float_bound_inclusive 1.0 in
  Gen.map (fun p -> int_of_float (-. (mean *. (log p)) +. skew)) unit_gen

(* This is an adaption of [QCheck.Shrink.list_spine]
       with more base cases added *)
let rec shrink_list_spine l yield =
  let rec split l len acc = match len,l with
    | _,[]
    | 0,_ -> List.rev acc, l
    | _,x::xs -> split xs (len-1) (x::acc) in
  match l with
  | [] -> ()
  | [_] -> yield []
  | [x;y] -> yield []; yield [x]; yield [y]
  | [x;y;z] -> yield [x]; yield [x;y]; yield [x;z]; yield [y;z]
  | [x;y;z;w] -> yield [x;y;z]; yield [x;y;w]; yield [x;z;w]; yield [y;z;w]
  | _::_ ->
    let len = List.length l in
    let xs,ys = split l ((1 + len) / 2) [] in
    yield xs;
    shrink_list_spine xs (fun xs' -> yield (xs'@ys))

(* This is an adaption of [QCheck.Shrink.list] *)
let shrink_list ?shrink l yield =
  shrink_list_spine l yield;
  match shrink with
  | None -> () (* no elem. shrinker provided *)
  | Some shrink -> Shrink.list_elems shrink l yield

let arb_cmds s =
  let cmds_gen = DLS_STM_seq.gen_cmds_size DLSConf.arb_cmd s (Gen.return 1000)(*exp_dist_gen*) in
  let shrinker = shrink_list ?shrink:(DLSConf.arb_cmd s).shrink in (* pass opt. elem. shrinker *)
  let ac = QCheck.make ~shrink:(Shrink.filter (DLS_STM_seq.cmds_ok DLSConf.init_state) shrinker) cmds_gen in
  (match (DLSConf.arb_cmd s).print with
   | None   -> ac
   | Some p -> set_print (Util.print_vertical p) ac)

let iter = ref 1

let agree_test ~count ~name =
  Test.make ~name ~count (arb_cmds DLSConf.init_state)
    (fun cs ->
       Printf.printf "%i %!" !iter;
     (*Printf.printf "%4i: %s\n%!" !iter Print.(list DLSConf.show_cmd cs);*)
       incr iter;
       agree_prop cs)

;;
QCheck_base_runner.run_tests_main [
  agree_test         ~count:1000 ~name:"STM Domain.DLS test sequential";
]
