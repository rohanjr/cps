type variable = Variable of string

type typ =
  | Unit
  | Arrow of typ * typ

type term =
  | Trivial of trivial
  | LetApp of variable * trivial * trivial (* serious *)

and trivial =
  | U
  | Var of variable
  | Lam of (variable * typ) * term
