(* Opérateurs binaires *)
type op = Plus | Minus | Times | Div | Inf | InfEq | Sup | SupEq | Equal ;;

(* Opérateurs binaires paresseux *)
type opl = Or | And ;;

type expr = 
    Var of string
  | Num of int
  | Bool of bool
  | Fun of string * expr 
  | App of expr * expr
  | Let of string * expr * expr  
  | Cond of expr * expr * expr
  | Op of op * expr * expr
  | OpLazy of opl * expr * expr
  | Not of expr
;;

(* Symboles correspondant aux opérateurs *)
let op_tostring op = match op with
  | Plus -> " + "
  | Minus -> " - "
  | Times -> " * " 
  | Div -> " / " 
  | Inf -> " < " 
  | InfEq -> " <= " 
  | Sup -> " > " 
  | SupEq -> " >= " 
  | Equal -> " == "
;;

(* Symboles correspondant aux opérateurs (paresseux)  *)
let opl_tostring opl = match opl with
   | Or -> " || " 
   | And -> " && "
;;

let rec tostring e = match e with
  | Var s -> s
  | Num i -> string_of_int i
  | Bool true -> "true"
  | Bool false -> "false"
  | App (e1, e2) -> "(" ^ tostring e1 ^ " " ^ tostring e2 ^ ")"
  | Fun (x, b) -> "fun " ^ x ^ " -> " ^ tostring b
  | Let (y, e1, e2) -> "let " ^ y ^ " = " ^ tostring e1 ^ " in " ^ tostring e2
  | Cond (e1,e2,e3) -> "if " ^ tostring e1 ^ " then " ^ tostring e2 ^ " else " ^ tostring e3 
  | Op (op, e1, e2) -> tostring e1 ^ op_tostring op ^ tostring e2
  | OpLazy (opl, e1, e2) -> tostring e1 ^ opl_tostring opl ^ tostring e2
  | Not (e1) -> "not (" ^ tostring e1 ^ ")"

;;
  
