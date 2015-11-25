open Variable

module N = Named
module S = Sequential

let rec sequential_of_named_term : N.term -> S.term = function
  | N.Trivial tr -> S.Trivial (sequential_of_named_trivial tr)
  | N.LetApp (app_var1, (N.LetApp (app_var2, app, tm1), tm2), tm3) ->
      sequential_of_named_term (N.LetApp (app_var2, app, N.LetApp (app_var1, (tm1, tm2), tm3)))
  | N.LetApp (app_var1, (N.Trivial tr, N.LetApp (app_var2, app, tm1)), tm2) ->
      sequential_of_named_term (N.LetApp (app_var2, app,
                                          N.LetApp (app_var1, (N.Trivial tr, tm1), tm2)))
  | N.LetApp (app_var, (N.Trivial tr1, N.Trivial tr2), tm) ->
      S.LetApp (app_var, (sequential_of_named_trivial tr1, sequential_of_named_trivial tr2),
                sequential_of_named_term tm)

and sequential_of_named_trivial : N.trivial -> S.trivial = function
  | N.U -> S.U
  | N.SrcVar src_var -> S.SrcVar src_var
  | N.AppVar app_var -> S.AppVar app_var
  | N.Lam (decl, tm) -> S.Lam (decl, sequential_of_named_term tm)
