open Variable2

type term =
  | U
  | Var of variable
  | Lam of variable * term
  | App of term * term
