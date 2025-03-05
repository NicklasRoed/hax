module Ocamltest
#set-options "--fuel 0 --ifuel 1 --z3rlimit 15"
open Core
open FStar.Mul

let main (_: Prims.unit) : Prims.unit =
  let _:i32 = mk_i32 2 +! mk_i32 2 in
  ()
