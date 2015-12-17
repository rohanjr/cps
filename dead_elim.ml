open Variable2
open Cps2

let count_if_equal (x : 'a) (y : 'a) : int = if x = y then 1 else 0

let rec num_free_occ_in_value (name : string) : value -> int = function
  | U -> 0
  | Lam (ContVariable cont_name, Variable var_name, tm) ->
      if name = cont_name || name = var_name then 0
      else num_free_occ_in_term name tm

and num_free_occ_in_term (name : string) : term -> int = function
  | LetVal (Variable var_name, v, tm) ->
      num_free_occ_in_value name v +
      if name = var_name then 0 else num_free_occ_in_term tm
  | LetCont (ContVariable cont_name, Variable var_name, tm1, tm2) ->
      let num_free_occ_in_tm1 =
        if name = var_name then 0
        else num_free_occ_in_term name tm1 in
      let num_free_occ_in_tm2 =
        if name = cont_name then 0
        else num_free_occ_in_term name tm2 in
      num_free_occ_in_tm1 + num_free_occ_in_tm2
  | ContApp (ContVariable cont_name, Variable var_name) ->
      count_if_equal name cont_name + count_if_equal name var_name
  | FuncApp (Variable func_name, ContVariable cont_name, Variable arg_name) ->
      count_if_equal name func_name + count_if_equal name cont_name +
      count_if_equal name arg_name

let is_free_in_term (name : string) (tm : term) : bool =
  num_free_occ_in_term name tm <> 0

let rec dead_elim : term -> term = function
  | LetVal (Variable var_name, v, tm) ->
      let tm' = dead_elim tm in
      if is_free_in_term var_name tm
      then LetVal (Variable var_name, v, tm')
      else tm'
  | LetCont (ContVariable cont_name, var, tm1, tm2) ->
      if is_free_in_term cont_name tm2 then
        LetCont (ContVariable cont_name, var, dead_elim tm1, dead_elim tm2)
      else
        dead_elim tm1
  | tm -> tm
