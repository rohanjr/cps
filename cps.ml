(* TODO add different types of variables for values & conts? *)

type variable = Variable of string

type typ =
  | Unit
  | Arrow of typ * typ

type trivial =
  | U
  | Var of variable
  | Lam of variable * variable * serious

and serious =
  | Cont of variable * trivial
  | KApp of (trivial * trivial) * (variable * serious)
  | Let of (variable * trivial) * serious

type program =
  | Program of variable * serious
