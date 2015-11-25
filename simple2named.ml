open Variable

module S = Simple
module N = Named

let rec named_of_simple_term : S.term -> N.term = function
  | S.U -> N.Trivial N.U
  | S.Var src_var -> N.Trivial (N.SrcVar src_var)
  | S.Lam (decl, tm) -> N.Trivial (N.Lam (decl, named_of_simple_term tm))
  | S.App (tm1, tm2) ->
      let app_var = get_fresh_app_var () in
      N.LetApp (app_var,
                (named_of_simple_term tm1, named_of_simple_term tm2),
                N.Trivial (N.AppVar app_var))
