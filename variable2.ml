(* Different kinds of variables *)

(* from lambdas in the original program or generated for naming intermediate
 * computations in the CPS language *)
type variable = Variable of string
(* continuation variables k *)
type cont_variable = ContVariable of string

(* Fresh variable generation *)

(* kind of variable to be generated *)
type gen_kind = Var | ContVar

(* counters for fresh names *)
let var_counter : int ref = ref 0
let cont_var_counter : int ref = ref 0

let get_fresh_name (var_kind : gen_kind) : string =
  let prefix, counter = match var_kind with
  | Var -> "_x", var_counter
  | ContVar -> "_k", cont_var_counter in
  let suffix = string_of_int !counter in
  counter := !counter + 1;
  prefix ^ suffix

let get_fresh_var () : variable = Variable (get_fresh_name Var)
let get_fresh_cont_var () : cont_variable = ContVariable (get_fresh_name ContVar)
