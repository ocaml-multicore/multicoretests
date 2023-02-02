open QCheck
open STM

(** parallel STM tests of Hashtbl *)

module HashtblModel =
struct
  type sut = (char, int) Hashtbl.t
  type state = (char * int) list
  type cmd =
    | Add of char * int
    | Remove of char
    | Find of char
    | Mem of char
    | Length [@@deriving show { with_path = false }]

  let init_sut () = Hashtbl.create ~random:false 42
  let cleanup (_:sut) = ()

  let arb_cmd (s:state) =
    let char =
      if s=[]
      then Gen.printable
      else Gen.(oneof [oneofl (List.map fst s); printable]) in
    let int = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [Gen.map2 (fun k v -> Add (k,v)) char int;
          Gen.map  (fun k   -> Remove k) char;
          Gen.map  (fun k   -> Find k) char;
          Gen.map  (fun k   -> Mem k) char;
          Gen.return Length; ])

  let next_state (c:cmd) (s:state) = match c with
    | Add (k,v) -> (k,v)::s
    | Remove k  -> List.remove_assoc k s
    | Find _
    | Mem _
    | Length    -> s

  let run (c:cmd) (h:sut) = match c with
    | Add (k,v) -> Res (unit, Hashtbl.add h k v)
    | Remove k  -> Res (unit, Hashtbl.remove h k)
    | Find k    -> Res (result int exn, protect (Hashtbl.find h) k)
    | Mem k     -> Res (bool, Hashtbl.mem h k)
    | Length    -> Res (int,  Hashtbl.length h)

  let init_state = []

  let precond (_:cmd) (_:state) = true
  let postcond (c:cmd) (s:state) (res:res) = match c,res with
    | Add (_,_), Res ((Unit,_),_)
    | Remove _,  Res ((Unit,_),_) -> true
    | Find k,    Res ((Result (Int,Exn),_),r) -> r = (try Ok (List.assoc k s) with Not_found -> Error Not_found)
    | Mem k,     Res ((Bool,_),r) -> r = List.mem_assoc k s
    | Length,    Res ((Int,_),r)  -> r = List.length s
    | _ -> false
end

module HT_seq = STM_sequential.Make(HashtblModel)
module HT_dom = STM_domain.Make(HashtblModel)
;;
QCheck_base_runner.run_tests_main
  (let count = 200 in
   [HT_seq.agree_test     ~count ~name:"Hashtbl test sequential";
    HT_dom.agree_test_par ~count ~name:"Hashtbl test parallel"; ])
