open Variable

type serious =
  | Cont of cont_variable * trivial
  | KApp of (trivial * trivial) * (app_variable * serious)
  (*| Let of (src_variable * trivial) * serious *)

and trivial =
  | U
  | SrcVar of src_variable
  | AppVar of app_variable
  | Lam of src_variable * cont_variable * serious

type program =
  | Program of cont_variable * serious
