
let _ =

  (* on définit l'entrée stadard comme buffer dans lequel lire les lexèmes *)
  let lexbuf = Lexing.from_channel stdin in

    (* on lance l'analyseur syntaxique avec l'analyseur lexical sur le buffer créé 
    et on récupère l'abre de syntaxe abstraite p *)
  let p = Parser.main Lexer.token lexbuf

    (* on évalue p, puis on traduit le résultat en chaine de caractères et on l'affiche *)
  in 
    if Eval.is_prog [] p
    
    (* On appelle simplify avant eval *)
    then print_endline (Expr.tostring (Eval.eval ( Eval.simplify p))) 
    else failwith "il existe des variables non liees" ;; 

