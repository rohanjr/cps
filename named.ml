type variable = Variable of string

type typ =
  | Unit
  | Arrow of typ * typ

type term =
  (* trivial terms *)
  | U
  | Var of variable
  | Lam of (variable * typ) * term
  (* serious terms *)
  | LetApp of variable * term * term
