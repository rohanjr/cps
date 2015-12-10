open Variable
open Simple

(* Running example in Danvy's 3 steps paper *)
let g = SrcVariable "g"
let x = SrcVariable "x"
let z = SrcVariable "z"
let eg = Lam ((g, Arrow (Unit, Arrow(Arrow(Unit, Unit), Unit))),
              Lam ((x, Unit), App (App (Var g, Var x), (Lam ((z, Unit), Var z)))))
let named_eg = Simple2named.named_of_simple_term eg
let sequential_eg = Named2sequential.sequential_of_named_term named_eg
let cps_eg = Sequential2cps.cps_program_of_sequential_term sequential_eg
let cps_eg' = Simple2cps.cps_program_of_simple_term eg (* same but with different variable names! *)

(* Shows that output is free of administrative redexes *)
let xx = Lam ((x, Unit), Var x)
let w = App (xx, xx)
let cps_w = Simple2cps.cps_program_of_simple_term w

(* Free variable test *)
let u = App (xx, Var x)
let cps_u = Simple2cps.cps_program_of_simple_term u
let x_free_in_u = Cps.num_occurrences_in_program "x" cps_u
let y_free_in_u = Cps.num_occurrences_in_program "y" cps_u
let g_free_in_u = Cps.num_occurrences_in_program "g" cps_u
