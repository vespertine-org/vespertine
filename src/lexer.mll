{
  open Lexing
  open Parser

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                pos_lnum = pos.pos_lnum + 1
      }
}

let white   = [' ' '\t']+
let digit   = ['0'-'9']
let int     = '-'? digit+
let letter  = ['a'-'z' 'A'-'Z' ]
let id      = (letter | '_') (letter|digit|'_')*
let newline = '\r' | '\n' | "\r\n"

rule read = 
  parse
  | white { read lexbuf } 
  | "true" { TRUE }
  | "false" { FALSE }
  | ":" { COLON }
  | "*" { TIMES }
  | "+" { PLUS }
  | "|" { BAR }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "let" { LET }
  | "=" { EQUALS }
  | "in" { IN }
  | "be" { BE }
  | "do" { DO }
  | "end" { END }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | newline { next_line lexbuf; read lexbuf }
  | eof { EOF }
  | _ {raise (SyntaxError ("Lexer - Illegal character: " ^ Lexing.lexeme lexbuf)) }
