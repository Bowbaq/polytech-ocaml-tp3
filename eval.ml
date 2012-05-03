open Expr ;;
open List ;;

(* Vérifie que l'entrée est bien un programme valide *)
let rec is_prog s e = match e with
  (* On verifie que la variable est bien dans la liste *)
  | Var x -> if (List.mem x s) then true else false
  | Num _ -> true
  | Bool _ -> true
  
  (* On ajoute x à la liste *)
  | Fun (x,e) -> is_prog (x::s) e
  | App (e1,e2) -> is_prog s e1 && is_prog s e2
  | Let (x, e1, e2) -> is_prog s e1 && is_prog (x::s) e2
  
  | Cond (e1,e2,e3) -> is_prog s e1 && is_prog s e2 && is_prog s e3
  
  | Op (op, e1, e2) -> is_prog s e1 && is_prog s e2
  | OpLazy (opl, e1, e2) -> is_prog s e1 && is_prog s e2
  | Not e -> is_prog s e
;;

(* Simplifie certaines expressions avant l'évaluation *)
let rec simplify e = match e with
  | Var y -> Var y
  | Num f -> Num f
  | Bool b -> Bool b
  
  | App (e1, e2) -> App( simplify e1, simplify e2)
  | Fun (y,b) -> Fun(y, simplify b)
  | Let (y, e1, e2) -> Let (y, simplify e1, simplify e2)
  
  | Cond (e1, e2, e3) -> (
  	match (simplify e1, simplify e2, simplify e3) with
  	    (* if e then true else false -> e  *)
  		| (ee1, Bool true, Bool false) -> ee1
  		(* if e1 then e2 else false -> e1 && e2 *)
  		| (ee1, ee2, Bool false) -> OpLazy(And, ee1, ee2)
		| (ee1, ee2, ee3) -> Cond(ee1, ee2, ee3)	
  		)
  
  | Op (op, e1, e2) -> Op(op, simplify e1, simplify e2)
  | OpLazy (opl, e1, e2) -> OpLazy(opl, simplify e1, simplify e2)
  | Not e1 -> Not (simplify e1)
;;

let rec subst x v e = match e with
  | Var y -> if x = y then v else (Var y)
  | Num f -> Num f
  | Bool b -> Bool b
  
  | App (e1, e2) -> App( subst x v e1, subst x v e2)   
  | Fun (y,b) -> 
      if x = y 
      then Fun (y,b) 
      else Fun(y, subst x v b)
  (* Le let in est une construction à part entière du langage *)     
  | Let (y, e1, e2) ->
  		if x = y
		then Let (y, subst x v e1, e2)
		else Let (y, subst x v e1, subst x v e2)

  | Cond (e1, e2, e3) -> Cond ( subst x v e1, subst x v e2, subst x v e3 ) 

  | Op (op, e1, e2) -> Op(op, subst x v e1, subst x v e2)
  | OpLazy (opl, e1, e2) -> OpLazy(opl, subst x v e1, subst x v e2) 

  | Not e1 -> Not (subst x v e1)
;;

let rec eval ex = match ex with 
  | Var v ->  failwith "Variable non liée."
  | Num f -> Num f
  | Bool b -> Bool b
  
  | App (e1,e2) ->
      let r1 = eval e1 in
      let r2 = eval e2 in
	(match r1 with
	   | Fun(s,e) -> 
	       let b = subst s r2 e in eval b
	   | _ -> failwith "erreur de type : fonction attendue"
	)
  | Fun (s,e) -> Fun (s,e)
  | Let (x, e1, e2) ->
  		let r1 = eval e1 in
		eval (subst x r1 e2)
  | Cond (eif, ethen, eelse) -> (
      match (eval eif) with
  	    | Bool true  -> eval ethen
  	    | Bool false -> eval eelse
  	    | _     -> failwith "erreur de type : bool attendu"
        )
        
  (* opérateurs binaires : *)
  | Op (op, e1, e2) -> (
  	  match (eval e1, eval e2) with
  	  	| (Num f1, Num f2) -> 
  	  		( match (op) with
  	  			| Plus -> Num ( f1 + f2 )
  				| Minus -> Num ( f1 - f2 )
  				| Times -> Num ( f1 * f2 )
  				| Div -> Num ( f1 / f2 )
  				| Inf -> Bool ( f1 < f2 )
  				| InfEq -> Bool ( f1 <= f2 )
  				| Sup -> Bool ( f1 > f2 )
  				| SupEq -> Bool ( f1 >= f2 )
  				| Equal -> Bool ( f1 = f2 )
  			)
  	    | (Bool b1, Bool b2) ->
  	    	(match (op) with
  	    		| Equal -> Bool ( b1 = b2 )
  	    		| _ -> failwith "erreur de type (bool)"
  	    	)
  	    | _ -> failwith "erreur de type"	
    )
  (* Les opérateurs && et || sont définis à part, de manière paresseuse *)
  | OpLazy (opl, e1, e2) -> (
  	  match (opl) with
  	  	| Or -> ( match(eval e1, e2) with
            	| (Bool true, _) -> Bool true
            	| (Bool false, e2) -> eval e2
            	| _ -> failwith "erreur de type (||)"
                )
        | And -> ( match(eval e1, e2) with
            	| (Bool false, _) -> Bool false
            	| (Bool true, e2) -> eval e2
            	| _ -> failwith "erreur de type (&&)"
                )
     )
  | Not e -> (
     match eval e with
	    | Bool b -> Bool (not b)
	    | _ -> failwith "erreur de type : booleen attendu"
        )

;;

