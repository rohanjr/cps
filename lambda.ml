(* Named representation of variables *)
type variable = Variable of string

type typ =
  | Unit
  | Nat
  | Arrow of typ * typ
  | Cross of typ * typ

type context = (variable * typ) list

type term =
  | U
  | Zero
  | Succ of term
  | Var of variable
  | Lam of (variable * typ) * term
  | App of term * term
  | Pair of term * term
  | Fst of term
  | Snd of term
  | Let of variable * term * term

type nat = Z | S of nat

type value =
  | VU
  | VNat of nat
  | VLam of variable * term
  | VPair of value * value

type environment = (variable * value) list

(* Context/environment lookup *)

let rec lookup (key : 'key) : ('key * 'value) list -> 'value option = function
  | [] -> None
  | (k, v) :: kvs -> if k = key then Some v else lookup key kvs

(* Type inference *)

let rec typ_of_term (ctx : context) : term -> typ option = function
  | U -> Some Unit
  | Zero -> Some Nat
  | Succ tm ->
      begin match typ_of_term ctx tm with
      | Some Nat -> Some Nat
      | _ -> None
      end
  | Var var -> lookup var ctx
  | Lam ((var, tp) as decl, tm) ->
      begin match typ_of_term (decl :: ctx) tm with
      | None -> None
      | Some tp' -> Some (Arrow (tp, tp'))
      end
  | App (tm1, tm2) ->
      begin match typ_of_term ctx tm1 with
      | Some (Arrow (tp1, tp2)) ->
          begin match typ_of_term ctx tm2 with
          | None -> None
          | Some tp -> if tp = tp1 then Some tp2 else None
          end
      | _ -> None
      end
  | Pair (tm1, tm2) ->
      begin match typ_of_term ctx tm1, typ_of_term ctx tm2 with
      | Some tp1, Some tp2 -> Some (Cross (tp1, tp2))
      | _, _ -> None
      end
  | Fst tm ->
      begin match typ_of_term ctx tm with
      | Some (Cross (tp1, _)) -> Some tp1
      | _ -> None
      end
  | Snd tm ->
      begin match typ_of_term ctx tm with
      | Some (Cross (_, tp2)) -> Some tp2
      | _ -> None
      end
  | Let (var, tm1, tm2) ->
      begin match typ_of_term ctx tm1 with
      | None -> None
      | Some tp -> typ_of_term ((var, tp) :: ctx) tm2
      end

let infer_type : term -> typ option = typ_of_term []

(* Environment-based evaluation *)

let rec value_of_term (env : environment) : term -> value option = function
  | U -> Some VU
  | Zero -> Some (VNat Z)
  | Succ tm ->
      begin match value_of_term env tm with
      | Some (VNat n) -> Some (VNat (S n))
      | _ -> None
      end
  | Var var -> lookup var env
  | Lam ((var, _), tm) -> Some (VLam (var, tm))
  | App (tm1, tm2) ->
      begin match value_of_term env tm1 with
      | Some (VLam (var, tm)) ->
          begin match value_of_term env tm2 with
          | None -> None
          | Some v -> value_of_term ((var, v) :: env) tm
          end
      | _ -> None
      end
  | Pair (tm1, tm2) ->
      begin match value_of_term env tm1, value_of_term env tm2 with
      | Some v1, Some v2 -> Some (VPair (v1, v2))
      | _, _ -> None
      end
  | Fst tm ->
      begin match value_of_term env tm with
      | Some (VPair (v1, _)) -> Some v1
      | _ -> None
      end
  | Snd tm ->
      begin match value_of_term env tm with
      | Some (VPair (_, v2)) -> Some v2
      | _ -> None
      end
  | Let (var, tm1, tm2) ->
      begin match value_of_term env tm1 with
      | None -> None
      | Some v -> value_of_term ((var, v) :: env) tm2
      end

let evaluate : term -> value option = value_of_term []

(* Examples *)

let tu = infer_type U
let t0 = infer_type Zero
let t2 = infer_type (Succ (Succ Zero))
let t12 = infer_type (Pair (Succ Zero, Succ (Succ Zero)))
let tinc = infer_type (Lam ((Variable "x", Nat), Var (Variable "x")))

let vu = evaluate U
let v0 = evaluate Zero
let v2 = evaluate (Succ (Succ Zero))
let v12 = evaluate (Pair (Succ Zero, Succ (Succ Zero)))
let vinc = evaluate (Lam ((Variable "x", Nat), Var (Variable "x")))
