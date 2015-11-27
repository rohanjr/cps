open Variable

(* CPS language *)

type serious =
  | Cont of cont_variable * trivial
  | KApp of (trivial * trivial) * (app_variable * serious)

and trivial =
  | U
  | SrcVar of src_variable
  | AppVar of app_variable
  | Lam of (src_variable * typ) * cont_variable * serious

type program =
  | Program of cont_variable * serious

(* Free variables TODO TEST! *)

let rec variable_free_in_serious (var_name : string) : serious -> bool = function
  | Cont (ContVariable name, tr) ->
      var_name <> name && variable_free_in_trivial var_name tr
  | KApp ((tr1, tr2), (AppVariable name, ser)) ->
      variable_free_in_trivial var_name tr1 ||
      variable_free_in_trivial var_name tr2 ||
      (var_name <> name && variable_free_in_serious var_name ser)

and variable_free_in_trivial (var_name : string) : trivial -> bool = function
  | U -> false
  | SrcVar (SrcVariable name) | AppVar (AppVariable name) -> var_name = name
  | Lam ((SrcVariable name1, _), ContVariable name2, ser) ->
      var_name <> name1 && var_name <> name2 && variable_free_in_serious var_name ser

let variable_free_in_program (var_name : string) : program -> bool = function
  | Program (ContVariable name, ser) ->
      var_name <> name && variable_free_in_serious var_name ser

(* Type checking *)

(*
type variable =
  | SrcVar of src_variable
  | AppVar of app_variable
  | ContVar of cont_variable

type context = (variable * typ) list
(* declarations that a src_var/app_var has type tp OR that a cont_var takes
 * inputs of type tp *)

let rec check_typ_of_program (ctx : context) (tp : typ) : term -> bool = function
  | Program (cont_var, ser) ->
      check_serious ((ContVar cont_var, tp) :: ctx) ser

and check_serious (ctx : context) : serious -> bool = function
  | Cont (cont_var, tr) -> 
  | KApp ((tr1, tr2), (app_var, ser)) -> 
      *)
