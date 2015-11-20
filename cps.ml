(* TODO add different types of variables for values & conts? *)

type variable = Variable of string

type serious =
  | Cont of variable * trivial
  | KApp of (trivial * trivial) * (variable * serious)
  | Let of (variable * trivial) * serious

and trivial =
  | U
  | Var of variable
  | Lam of variable * variable * serious

type program =
  | Program of variable * serious
