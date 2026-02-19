open Lin

module Eio_backend = struct
  type context = Dm : [> Eio.Domain_manager.ty ] Eio.Resource.t -> context
  let spawn_pair (Dm dm) f g =
    Eio.Fiber.pair
      (fun () -> Eio.Domain_manager.run dm f)
      (fun () -> Eio.Domain_manager.run dm g)
end

module Make_internal (Spec : Internal.CmdSpec [@alert "-internal"]) = struct
  module Common = Lin_domain_common.Make_internal(Spec)(Eio_backend)
  include Common
  let lin_prop ~domain_mgr = Common.lin_prop ~ctx:(Eio_backend.Dm domain_mgr)
  let stress_prop ~domain_mgr = Common.stress_prop ~ctx:(Eio_backend.Dm domain_mgr)
  let lin_test ~domain_mgr = Common.lin_test ~ctx:(Eio_backend.Dm domain_mgr)
  let neg_lin_test ~domain_mgr = Common.neg_lin_test ~ctx:(Eio_backend.Dm domain_mgr)
  let stress_test ~domain_mgr = Common.stress_test ~ctx:(Eio_backend.Dm domain_mgr)
end

module Make (Spec : Spec) = Make_internal(MakeCmd(Spec))
