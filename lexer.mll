 {
    open Parser        (* The type token is defined in parser.mli *)
	exception Eof
	  
 }
  rule token = parse
      [' ' '\t']     { token lexbuf }     (* skip blanks *)
    | ['\n' ]        { token lexbuf }
    | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
    | ['a'-'z']+ as lxm { match lxm with 
			    | "if"    -> IF 
			    | "then"  -> THEN
			    | "else"  -> ELSE
			    | "fi"    -> FI
			    | "let"   -> LET
			    | "rec"   -> REC 
			    | "in"    -> IN
			    | "fun"   -> FUN
			    | "true"  -> TRUE
			    | "false" -> FALSE
			    | "not"   -> NOT
			    | "cond"  -> COND
			    | "dnoc"  -> DNOC
				
			    | _ -> IDENT(lxm) }

    | "-->"          { AARROW } (* pour la conditionnelle multiple *)
    | '|'            { PIPE  }  (* pour la conditionnelle multiple *)
    | '_'            { ANY }    (* pour la conditionnelle multiple *)

    | "->"           { ARROW }
    | '+'            { OP ( Expr.Plus )   }
    | '-'            { OP ( Expr.Minus )  }
    | '*'            { OP ( Expr.Times )  }
    | '/'            { OP ( Expr.Div )    }
    | '>'            { OP ( Expr.Sup )    }
    | ">="           { OP ( Expr.SupEq )  }
    | '<'            { OP ( Expr.Inf )    }
    | "<="           { OP ( Expr.InfEq )  }
    | "||"           { OPLAZY ( Expr.Or )     }
    | "&&"           { OPLAZY ( Expr.And )    }
    | "=="           { OP ( Expr.Equal )  }
    

    | '('            { LPAREN }
    | ')'            { RPAREN }
    | '='            { BE }
    | eof            { EOF }
	
	
