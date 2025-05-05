module Translations
#set-options "--fuel 2 --ifuel 2 --z3rlimit 30"
open FStar.Mul
open FStar.Int32

class t_Measurable (v_Self: Type0) = {
  f_measure_pre: v_Self -> Type0;
  f_measure_post: v_Self -> Int32.t -> Type0;
  f_measure: x0: v_Self -> Prims.Pure Int32.t (f_measure_pre x0) (fun result -> f_measure_post x0 result)
}

let get_measurement
      (#v_T: Type0)
      (#[FStar.Tactics.Typeclasses.tcresolve ()] tc: t_Measurable v_T)
      (item: v_T)
    : Pure Int32.t 
      (requires tc.f_measure_pre item)
      (ensures (fun res -> tc.f_measure_post item res))
    = tc.f_measure item

let main (_: Prims.unit) : Prims.unit = ()