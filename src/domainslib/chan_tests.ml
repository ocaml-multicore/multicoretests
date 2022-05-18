open QCheck
open Domainslib

(** This is a parallel test of Domainslib.Chan *)

module ChConf =
struct
  type state = int list
  type sut = int Domainslib.Chan.t
  type cmd =
    | Send of int
    | Send_poll of int
    | Recv
    | Recv_poll [@@deriving show { with_path = false }]

  let capacity = 8

  let arb_cmd s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (if s=[]
       then
         Gen.oneof
           [Gen.map (fun i -> Send i) int_gen;
	    Gen.map (fun i -> Send_poll i) int_gen;
	    Gen.return Recv_poll] (* don't generate blocking Recv cmds on an empty channel *)
       else
       if List.length s >= capacity
       then
         Gen.oneof
           [Gen.map (fun i -> Send_poll i) int_gen;
            Gen.return Recv;
	    Gen.return Recv_poll] (* don't generate blocking Send cmds on a full channel *)
       else
         Gen.oneof
           [Gen.map (fun i -> Send i) int_gen;
	    Gen.map (fun i -> Send_poll i) int_gen;
            Gen.return Recv;
	    Gen.return Recv_poll])
  let init_state  = []
  let init_sut () = Chan.make_bounded capacity
  let cleanup _   = ()

  let next_state c s = match c with
    | Send i      -> if List.length s < capacity then s@[i] else s
    | Send_poll i -> if List.length s < capacity then s@[i] else s
    | Recv        -> begin match s with [] -> [] | _::s' -> s' end
    | Recv_poll   -> begin match s with [] -> [] | _::s' -> s' end

  let precond c s = match c,s with
    | Recv,   [] -> false
    | Send _, _  -> List.length s < capacity
    | _,      _  -> true

  type res = RSend | RSend_poll of bool | RRecv of int | RRecv_poll of int option [@@deriving show { with_path = false }]

  let run c chan =
    match c with
    | Send i       -> (Chan.send chan i; RSend)
    | Send_poll i  -> RSend_poll (Chan.send_poll chan i)
    | Recv         -> RRecv (Chan.recv chan)
    | Recv_poll    -> RRecv_poll (Chan.recv_poll chan)

  let postcond c s res = match c,res with
    | Send _,      RSend          -> (List.length s < capacity)
    | Send_poll _, RSend_poll res -> res = (List.length s < capacity)
    | Recv,        RRecv res      -> (match s with [] -> false | res'::_ -> res=res')
    | Recv_poll,   RRecv_poll opt -> (match s with [] -> None | res'::_ -> Some res') = opt
    | _,_ -> false
end


module ChT = STM.Make(ChConf)
;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main
  (let count,name = 1000,"global Domainslib.Chan test" in [
      ChT.agree_test     ~count ~name;
      ChT.agree_test_par ~count ~name;
    ])
