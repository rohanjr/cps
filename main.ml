open Variable
open Simple

let u = U
let cps_u = Simple2cps.cps_program_of_simple_term u

(* Running example in Danvy's 3 steps paper *)
let g = SrcVariable "g"
let x = SrcVariable "x"
let z = SrcVariable "z"
let eg = Lam ((g, Unit), Lam ((x, Unit), App (App (Var g, Var x), (Lam ((z, Unit), Var z)))))
let named_eg = Simple2named.named_of_simple_term eg
let sequential_eg = Named2sequential.sequential_of_named_term named_eg
let cps_eg = Sequential2cps.cps_program_of_sequential_term sequential_eg
let cps_eg' = Simple2cps.cps_program_of_simple_term eg (* same but with different variable names! *)
