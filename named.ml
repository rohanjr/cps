type variable = Variable of string

type typ =
  | Unit
  | Arrow of typ * typ

type term =
  | Trivial of trivial
  (* serious term: named application between terms *)
  (* let var = e1 e2 in tm *)
  | LetApp of variable * (term * term) * term

and trivial =
  | U
  | Var of variable
  | Lam of (variable * typ) * term
