(* This file uses some advanced parsing techniques
   to parse juxtaposed applications [e1 e2 e3] the
	 same way as OCaml does. *)

%{
open Ast

(** [make_apply e [e1; e2; ...]] makes the application  
    [e e1 e2 ...]).  Requires: the list argument is non-empty. *)
let rec make_apply e = function
  | [] -> failwith "precondition violated"
  | [e'] -> App (e, e')
	| h :: ((_ :: _) as t) -> make_apply (App (e, h)) t
%}

%token <string> ID
%token <int> INT
%token TRUE
%token FALSE
%token COLON
%token TIMES  
%token PLUS
%token BAR
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LET
%token EQUALS
%token IN
%token BE
%token DO
%token END
%token IF
%token THEN
%token ELSE
%token EOF

%nonassoc IN
%nonassoc ELSE
%left PLUS
%left STAR  

%start <Ast.expr> prog

%%

prog:
	| e = expr; EOF { e }
	;
	
expr:
	| e = simpl_expr { e } 
	| e = simpl_expr; es = simpl_expr+ { make_apply e es }
  | DO; COLON; BAR; x = ID ; BAR; e = expr { Fun (x, e) }
  | DO; BAR; x = ID ; BAR; e = expr; END { Fun (x, e) }
	;

simpl_expr:
	| x = ID { Var x }
  | LPAREN; e=expr; RPAREN { e } 
  ;
