type typ =
  | Unit
  | Integer
  | Arrow of typ * typ
  | Cross of typ * typ

type variable = Variable of string

type context = (variable * typ) list
(*
  | CEmpty
  | CVar of context * variable
  *)

type term =
  | U
  | Int of int
  | Var of variable
  | Lam of (variable * typ) * term
  | App of term * term
  | Pair of term * term
  | Fst of term
  | Snd of term
  | Let of variable * term * term

let typ_of_variable (ctx : context) (var : variable) : typ option =
  match List.filter (fun (v, _) -> v = var) ctx with
  | [] -> None
  | (_, tp) :: _ -> Some tp

let rec typ_of_term (ctx : context) : term -> typ option = function
  | U -> Some Unit
  | Int _ -> Some Integer
  | Var var -> typ_of_variable ctx var
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

(* Examples *)
let tu = typ_of_term [] U
let t0 = typ_of_term [] (Int 0)
let t2_3 = typ_of_term [] (Pair (Int 2, Int 3))
let tinc = typ_of_term [] (Lam ((Variable "x", Integer), Var (Variable "x")))
