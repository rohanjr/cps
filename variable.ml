(* Different kinds of variables *)

type src_variable = SrcVariable of string (* from lambdas in the original program *)
type app_variable = AppVariable of string (* intermediate names v of applications, from the naming phase *)
type cont_variable = ContVariable of string (* continuation variables k, from the cps stage *)

(* Types of source variables *)
type typ =
  | Unit
  | Arrow of typ * typ


(* Fresh variable generation *)

(* kind of variable to be generated *)
type gen_kind = AppVar | ContVar

(* counters for fresh names *)
let app_counter : int ref = ref 0
let cont_counter : int ref = ref 0

let get_fresh_name (var_kind : gen_kind) : string =
  let prefix, counter = match var_kind with
  | AppVar -> "_v", app_counter
  | ContVar -> "_k", cont_counter in
  let suffix = string_of_int !counter in
  counter := !counter + 1;
  prefix ^ suffix

let get_fresh_app_var () : app_variable = AppVariable (get_fresh_name AppVar)
let get_fresh_cont_var () : cont_variable = ContVariable (get_fresh_name ContVar)
