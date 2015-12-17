open Variable2

type term =
  | LetVal of variable * value * term
  | LetCont of cont_variable * variable * term * term
  | ContApp of cont_variable * variable
  | FuncApp of variable * cont_variable * variable

and lambda = cont_variable * variable * term

and value =
  | U
  | Lam of lambda

type runtime_value =
  | RU
  | RLam of environment * lambda

and cont_value = environment * (variable * term)

and env_mapping =
  | RMap of variable * runtime_value
  | CMap of cont_variable * cont_value

and environment = env_mapping list

type env_variable =
  | Var of variable
  | ContVar of cont_variable

type env_value =
  | RVal of runtime_value
  | CVal of cont_value

let rec env_lookup (env : environment) (env_var : env_variable) : env_value option =
  match env, env_var with
  | [], _ -> None
  | RMap (var, rval) :: env', Var env_var
      when var = env_var -> Some (RVal rval)
  | CMap (cont_var, cval) :: env', ContVar env_cont_var
      when cont_var = env_cont_var -> Some (CVal cval)
  | _ :: env', env_var -> env_lookup env' env_var

let interpret (env : environment) : value -> runtime_value = function
  | U -> RU
  | Lam lam -> RLam (env, lam)

let rec evaluate (env : environment) : term -> runtime_value option =
  let lookup = env_lookup env in function
  | LetVal (var, v, tm) ->
      evaluate (RMap (var, interpret env v) :: env) tm
  | LetCont (cont_var, var, tm1, tm2) ->
      evaluate (CMap (cont_var, (env, (var, tm1))) :: env) tm2
  | ContApp (cont_var, var) ->
      begin match lookup (ContVar cont_var), lookup (Var var) with
      | Some (CVal (env', (var', tm))), Some (RVal rval) ->
          evaluate (RMap (var', rval) :: env') tm
      | _, _ -> None
      end
  | FuncApp (func_var, cont_var, var) ->
      begin match lookup (Var func_var),
                  lookup (ContVar cont_var),
                  lookup (Var var) with
      | Some (RVal (RLam (env', (cont_var', var', tm)))),
        Some (CVal cval), Some (RVal rval) ->
          evaluate (RMap (var', rval) :: CMap (cont_var', cval) :: env') tm
      | _, _, _ -> None
      end
