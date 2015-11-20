module N = Named
module S = Sequential

let rec sequential_of_named_term : N.term -> S.term = function
  | N.Trivial tr -> S.Trivial (sequential_of_named_trivial tr)
  | N.LetApp (var1, (N.LetApp (var2, app, tm1), tm2), tm3) ->
      sequential_of_named_term (N.LetApp (var2, app, N.LetApp (var1, (tm1, tm2), tm3)))
  | N.LetApp (var1, (N.Trivial tr, N.LetApp (var2, app, tm1)), tm2) ->
      sequential_of_named_term (N.LetApp (var2, app,
                                          N.LetApp (var1, (N.Trivial tr, tm1), tm2)))
  | N.LetApp (var, (N.Trivial tr1, N.Trivial tr2), tm) ->
      S.LetApp (var, (sequential_of_named_trivial tr1, sequential_of_named_trivial tr2),
                sequential_of_named_term tm)

and sequential_of_named_trivial : N.trivial -> S.trivial = function
  | N.U -> S.U
  | N.Var var -> S.Var var
  | N.Lam (decl, tm) -> S.Lam (decl, sequential_of_named_term tm)
