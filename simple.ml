open Variable

type term =
  | U
  | Var of src_variable
  | Lam of (src_variable * typ) * term
  | App of term * term
