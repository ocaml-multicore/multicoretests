open STM

module Domain_backend = struct
  type context = unit
  let spawn_pair () f g =
    let d1 = Domain.spawn f in
    let d2 = Domain.spawn g in
    (Domain.join d1, Domain.join d2)
end

module MakeExt (Spec : SpecExt) = struct
  module Common = STM_domain_common.MakeExt(Spec)(Domain_backend)
  include Common
  let agree_prop_par = Common.agree_prop_par ~ctx:()
  let stress_prop_par = Common.stress_prop_par ~ctx:()
  let agree_prop_par_asym = Common.agree_prop_par_asym ~ctx:()
  let agree_test_par = Common.agree_test_par ~ctx:()
  let stress_test_par = Common.stress_test_par ~ctx:()
  let neg_agree_test_par = Common.neg_agree_test_par ~ctx:()
  let agree_test_par_asym = Common.agree_test_par_asym ~ctx:()
  let neg_agree_test_par_asym = Common.neg_agree_test_par_asym ~ctx:()
end

module Make (Spec : Spec) =
  MakeExt (struct
    include SpecDefaults
    include Spec
  end)
