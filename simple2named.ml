module S = Simple
module N = Named

(* Generate fresh value variables v *)
(* TODO: make less crude *)
let counter : int ref = ref 0
let get_fresh_var () : N.variable =
  let fresh_name = "_v" ^ string_of_int !counter in
  counter := !counter + 1;
  N.Variable fresh_name

let rec named_of_simple_term : S.term -> N.term = function
  | S.U -> N.Trivial N.U
  | S.Var var -> N.Trivial (N.Var var)
  | S.Lam (decl, tm) -> N.Trivial (N.Lam (decl, named_of_simple_term tm))
  | S.App (tm1, tm2) ->
      let fresh_var = get_fresh_var () in
      N.LetApp (fresh_var,
                (named_of_simple_term tm1, named_of_simple_term tm2),
                N.Trivial (N.Var fresh_var))
