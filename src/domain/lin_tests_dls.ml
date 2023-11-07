open Domain
open Lin

module DLSConf = struct
  type t = int DLS.key
  let init () = DLS.new_key (fun () -> 0) (* without split from parent *)
  let cleanup _ = ()
  let int = int_small
  let api = [
    val_ "DLS.get" DLS.get (t @-> returning int) ;
    val_ "DLS.set" DLS.set (t @-> int @-> returning unit) ;
  ]
end

module DLSN = Lin_domain.Make (DLSConf)
module DLST = Lin_domain.Make (struct
    include DLSConf
    let init () = (* get and set will see the parent's key *)
      DLS.new_key ~split_from_parent:(fun i -> i) (fun () -> 0)
  end)
;;
(*
let _ =
  Domain.join (Domain.spawn (fun () -> QCheck_base_runner.run_tests ~verbose:true
                                [DLSN.neg_lin_test ~count:100 ~name:"Lin Domain.DLS negative test with Domain"]))
let _ =
  Domain.join (Domain.spawn (fun () -> QCheck_base_runner.run_tests ~verbose:true
                                [DLST.lin_test     ~count:75  ~name:"Lin Domain.DLS test with Domain"]))
*)
QCheck_base_runner.run_tests_main [
  DLSN.neg_lin_test ~count:100 ~name:"Lin Domain.DLS negative test with Domain";
  DLST.lin_test     ~count:75  ~name:"Lin Domain.DLS test with Domain";
]

