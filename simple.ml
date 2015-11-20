type variable = Variable of string

type typ =
  | Unit
  | Arrow of typ * typ

type context = (variable * typ) list

type term =
  | U
  | Var of variable
  | Lam of (variable * typ) * term
  | App of term * term

type value =
  | VU
  | VLam of variable * term
