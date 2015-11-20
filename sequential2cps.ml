module S = Sequential
module C = Cps

(* Generate fresh continuation variables k *)
(* TODO: make less crude *)
let counter : int ref = ref 0
let get_fresh_var () : C.variable =
  let fresh_name = "_k" ^ string_of_int !counter in
  counter := !counter + 1;
  C.Variable fresh_name

let rec cps_trivial_of_sequential_trivial : S.trivial -> C.trivial = function
  | S.U -> C.U
  | S.Var var -> C.Var var
  | S.Lam ((var, _), tm) ->
      let cont_var = get_fresh_var () in
      C.Lam (var, cont_var,
             cps_serious_of_sequential_term_cont
               (tm, fun tr -> C.Cont (cont_var, tr)))

and cps_serious_of_sequential_term_cont : S.term * (C.trivial -> C.serious) -> C.serious = function
  | S.Trivial S.U, k -> k C.U
  | S.Trivial (S.Var var), k -> k (C.Var var)
  | S.Trivial (S.Lam ((var, _), tm)), k ->
      let cont_var = get_fresh_var () in
      k (C.Lam (var, cont_var,
                cps_serious_of_sequential_term_cont
                  (tm, fun tr -> C.Cont (cont_var, tr))))
  | LetApp (var, (tr1, tr2), tm), k ->
      C.KApp ((cps_trivial_of_sequential_trivial tr1,
               cps_trivial_of_sequential_trivial tr2),
              (var, cps_serious_of_sequential_term_cont (tm, k)))

let cps_program_of_sequential_term (tm : S.term) : C.program =
  let cont_var = get_fresh_var () in
  C.Program (cont_var,
             cps_serious_of_sequential_term_cont (tm, fun tr -> C.Cont
               (cont_var, cps_trivial_of_sequential_trivial tr)))
