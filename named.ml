open Variable

type term =
  | Trivial of trivial
  (* serious term: named application between terms *)
  (* let v = e1 e2 in tm *)
  | LetApp of app_variable * (term * term) * term

and trivial =
  | U
  | SrcVar of src_variable
  | AppVar of app_variable
  | Lam of (src_variable * typ) * term
