open STM

module Eio_backend = struct
  type context = Dm : [> Eio.Domain_manager.ty ] Eio.Resource.t -> context
  let spawn_pair (Dm dm) f g =
    Eio.Fiber.pair
      (fun () -> Eio.Domain_manager.run dm f)
      (fun () -> Eio.Domain_manager.run dm g)
end

module MakeExt (Spec : SpecExt) = struct
  module Common = STM_domain_common.MakeExt(Spec)(Eio_backend)
  include Common
  let agree_prop_par ~domain_mgr = Common.agree_prop_par ~ctx:(Eio_backend.Dm domain_mgr)
  let stress_prop_par ~domain_mgr = Common.stress_prop_par ~ctx:(Eio_backend.Dm domain_mgr)
  let agree_prop_par_asym ~domain_mgr = Common.agree_prop_par_asym ~ctx:(Eio_backend.Dm domain_mgr)
  let agree_test_par ~domain_mgr = Common.agree_test_par ~ctx:(Eio_backend.Dm domain_mgr)
  let stress_test_par ~domain_mgr = Common.stress_test_par ~ctx:(Eio_backend.Dm domain_mgr)
  let neg_agree_test_par ~domain_mgr = Common.neg_agree_test_par ~ctx:(Eio_backend.Dm domain_mgr)
  let agree_test_par_asym ~domain_mgr = Common.agree_test_par_asym ~ctx:(Eio_backend.Dm domain_mgr)
  let neg_agree_test_par_asym ~domain_mgr = Common.neg_agree_test_par_asym ~ctx:(Eio_backend.Dm domain_mgr)
end

module Make (Spec : Spec) =
  MakeExt (struct
    include SpecDefaults
    include Spec
  end)
