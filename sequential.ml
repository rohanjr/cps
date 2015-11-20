(* TODO add different types of variables for values? *)
type variable = Variable of string

type typ =
  | Unit
  | Arrow of typ * typ

type term =
  | Trivial of trivial
  (* serious term: named application between trivial terms *)
  (* let var = tr2 tr2 in tm *)
  | LetApp of variable * (trivial * trivial) * term

and trivial =
  | U
  | Var of variable
  | Lam of (variable * typ) * term
