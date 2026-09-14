open Lin

module Domain_backend = struct
  type context = unit
  let spawn_pair () f g =
    let d1 = Domain.spawn f in
    let d2 = Domain.spawn g in
    (Domain.join d1, Domain.join d2)
end

module Make_internal (Spec : Internal.CmdSpec [@alert "-internal"]) = struct
  module Common = Lin_domain_common.Make_internal(Spec)(Domain_backend)
  include Common
  let lin_prop = Common.lin_prop ~ctx:()
  let stress_prop = Common.stress_prop ~ctx:()
  let lin_test = Common.lin_test ~ctx:()
  let neg_lin_test = Common.neg_lin_test ~ctx:()
  let stress_test = Common.stress_test ~ctx:()
end

module Make (Spec : Spec) = Make_internal(MakeCmd(Spec))
