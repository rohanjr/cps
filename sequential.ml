open Variable

type term =
  | Trivial of trivial
  (* serious term: named application between trivial terms *)
  (* let var = tr2 tr2 in tm *)
  | LetApp of app_variable * (trivial * trivial) * term

and trivial =
  | U
  | SrcVar of src_variable
  | AppVar of app_variable
  | Lam of (src_variable * typ) * term
