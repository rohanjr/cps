open Variable

module S = Sequential
module C = Cps

let rec cps_trivial_of_sequential_trivial : S.trivial -> C.trivial = function
  | S.U -> C.U
  | S.SrcVar src_var -> C.SrcVar src_var
  | S.AppVar app_var -> C.AppVar app_var
  | S.Lam (decl, tm) ->
      let cont_var = get_fresh_cont_var () in
      C.Lam (decl, cont_var,
             cps_serious_of_sequential_term_cont
               (tm, fun tr -> C.Cont (cont_var, tr)))

and cps_serious_of_sequential_term_cont : S.term * (C.trivial -> C.serious) -> C.serious = function
  | S.Trivial S.U, k -> k C.U
  | S.Trivial (S.SrcVar src_var), k -> k (C.SrcVar src_var)
  | S.Trivial (S.AppVar app_var), k -> k (C.AppVar app_var)
  | S.Trivial (S.Lam (decl, tm)), k ->
      let cont_var = get_fresh_cont_var () in
      k (C.Lam (decl, cont_var,
                cps_serious_of_sequential_term_cont
                  (tm, fun tr -> C.Cont (cont_var, tr))))
  | S.LetApp (app_var, (tr1, tr2), tm), k ->
      C.KApp ((cps_trivial_of_sequential_trivial tr1,
               cps_trivial_of_sequential_trivial tr2),
              (app_var, cps_serious_of_sequential_term_cont (tm, k)))

let cps_program_of_sequential_term (tm : S.term) : C.program =
  let cont_var = get_fresh_cont_var () in
  C.Program (cont_var,
             cps_serious_of_sequential_term_cont
               (tm, fun tr -> C.Cont (cont_var, tr)))
