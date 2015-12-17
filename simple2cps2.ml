open Variable2

module S = Simple2
module C = Cps2

let rec cps_term_of_simple_term (k : variable -> C.term) : S.term -> C.term = function
  | S.U -> let var = get_fresh_var () in C.LetVal (var, C.U, k var)
  | S.Var var -> k var
  | S.Lam (var, tm) ->
      let func_var = get_fresh_var () in
      let cont_var = get_fresh_cont_var () in
      C.LetVal (func_var,
                C.Lam (cont_var, var,
                       cps_term_of_simple_term (fun z -> C.ContApp (cont_var, z)) tm),
                k func_var)
  | S.App (tm1, tm2) ->
      let var = get_fresh_var () in
      let cont_var = get_fresh_cont_var () in
      cps_term_of_simple_term
        (fun z1 ->
          cps_term_of_simple_term
            (fun z2 -> C.LetCont (cont_var, var, k var, C.FuncApp (z1, cont_var, z2)))
            tm2)
        tm1
