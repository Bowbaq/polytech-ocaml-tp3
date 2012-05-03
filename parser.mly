
  /* Analyseur lexical pour le cours de programmation fonctionnelle SILR 4 Polytech'Nantes. */
  /* Certaines lignes reprises du code source de OCaml par X. Leroy */

%{ (* prelude en OCaml *)

  open Expr ;;

  let rec make_app e l = match l with
    | a :: [] -> App(e,a)
    | a :: r -> make_app (App (e,a)) r
    (* On a rajouté la ligne suivante pour enlever le warning de compilation *)
    | [] -> failwith "Liste vide"
 ;;

 %}
  

/*déclaration des tokens */

%token <int> INT
%token <string> IDENT
%token <Expr.op> OP
%token <Expr.opl> OPLAZY

  
%token IF THEN ELSE FI
%token TRUE FALSE NOT
%token LET REC BE IN 
%token LPAREN RPAREN
%token FUN ARROW
%token EOF
%token OP OPLAZY
%token COND DNOC AARROW ANY PIPE
  

/* gestion des priorités/associativités */
%left OP OPLAZY
%nonassoc IN
%nonassoc ARROW

  
%start main               /*  entry point    */
%type <Expr.expr> main
%%
  main:
  expr EOF                              { $1                        }
  ;
  
  expr:
  | simpleexpr                          { $1                        }
  | simpleexpr simpleexprlist           { make_app $1 (List.rev $2) } /* application associative a gauche */
  /* Opérateurs binaires */
  | expr OP expr                        { Op($2, $1, $3)            }
  /* Operateurs binaires paresseux */
  | expr OPLAZY expr                    { OpLazy($2, $1, $3)        }
  /* Implémentation du let en tant que construction du langage */  
  | LET IDENT BE expr IN expr           { Let( $2, $4, $6)          } 
  | REC IDENT IDENT BE expr             { failwith "recursion : implémentez-moi" } 
  | FUN IDENT ARROW expr                { Fun ($2,$4)               } 
  | NOT simpleexpr                      { Not ($2)                  } 
  ;
  
  simpleexpr:
  | LPAREN expr RPAREN                  { $2                        }
  | INT                                 { Num $1                    }
  | IDENT                               { Var $1                    }
  | TRUE                                { Bool true                 }
  | FALSE                               { Bool false                }
  | IF expr THEN expr ELSE expr FI      { Cond ($2, $4, $6)         } 

/* Conditionnelle multiple */
  | COND rulelist  ANY AARROW expr DNOC { failwith "conditionnelle multiple : implémentez-moi" }  
  ;
  
  simpleexprlist:
  | simpleexpr { [$1] }
  | simpleexprlist simpleexpr  { $2 :: $1 }
    /* cette règle permet d'avoir une associativite a gauche */


/* pour la conditionnelle multiple : */
  rulelist:
  | expr AARROW expr PIPE           { ($1,$3)::[] }
  | expr AARROW expr PIPE rulelist  { ($1,$3)::$5 }
;
