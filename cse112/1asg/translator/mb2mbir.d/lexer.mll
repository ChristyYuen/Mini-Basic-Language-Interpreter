(* $Id: lexer.mll,v 1.5 2020-08-30 19:36:37-07 - - $ *)

{
(******** BEGIN LEXER SEMANTICS ********)

open Absyn
open Etc
open Lexing
open Parser
open Printf

let lexerror lexbuf =
    lexeprint (lexeme_start_p lexbuf)
              ["invalid character `" ^ (lexeme lexbuf) ^ "'"]

let newline lexbuf =
    let incrline pos =
        {pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum}
    in  (lexbuf.lex_start_p <- incrline lexbuf.lex_start_p;
         lexbuf.lex_curr_p <- incrline lexbuf.lex_curr_p)

(*
let list lexbuf =
    let pos = lexeme_start_p lexbuf
    in  (if pos.pos_bol = pos.pos_cnum
         then printf ";;%4d: " pos.pos_lnum;
         printf "%s" (lexeme lexbuf))
*)

(******** END LEXER SEMANTICS ********)
}

let letter          = ['a'-'z' 'A'-'Z' '_']
let digit           = ['0'-'9']
let fraction        = (digit+ '.'? digit* | '.' digit+)
let exponent        = (['E' 'e'] ['+' '-']? digit+)

let comment         = ('#' [^'\n']*)
let ident           = (letter (letter | digit)*)
let number          = (fraction exponent?)
let string          = '"' [^'\n' '"']* '"'

rule token          = parse
    | eof           { EOF }
    | [' ' '\t']    { token lexbuf }
    | comment       { token lexbuf }
    | "\n"          { newline lexbuf; EOL }
    | ":"           { COLON }
    | ","           { COMMA }
    | "("           { LPAR }
    | ")"           { RPAR }
    | "["           { LSUB }
    | "]"           { RSUB }
    | "="           { EQUAL (lexeme lexbuf) }
    | "!="          { RELOP (lexeme lexbuf) }
    | "<"           { RELOP (lexeme lexbuf) }
    | "<="          { RELOP (lexeme lexbuf) }
    | ">"           { RELOP (lexeme lexbuf) }
    | ">="          { RELOP (lexeme lexbuf) }
    | "+"           { ADDOP (lexeme lexbuf) }
    | "-"           { ADDOP (lexeme lexbuf) }
    | "*"           { MULOP (lexeme lexbuf) }
    | "/"           { MULOP (lexeme lexbuf) }
    | "%"           { MULOP (lexeme lexbuf) }
    | "^"           { POWOP (lexeme lexbuf) }
    | "dim"         { DIM }
    | "goto"        { GOTO }
    | "if"          { IF }
    | "input"       { INPUT }
    | "let"         { LET }
    | "print"       { PRINT }
    | number        { NUMBER (lexeme lexbuf) }
    | string        { STRING (lexeme lexbuf) }
    | ident         { IDENT (lexeme lexbuf) }
    | _             { lexerror lexbuf; token lexbuf }

