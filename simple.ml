type variable = Variable of string

type typ =
  | Unit
  | Arrow of typ * typ

type term =
  | U
  | Var of variable
  | Lam of (variable * typ) * term
  | App of term * term
