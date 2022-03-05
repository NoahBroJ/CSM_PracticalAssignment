// Signature file for parser generated by fsyacc
module Parser
type token = 
  | TIMES
  | DIV
  | PLUS
  | MINUS
  | POW
  | LPAR
  | RPAR
  | ASSIGN
  | SKIP
  | SEMICOLON
  | IF
  | FI
  | DO
  | OD
  | PRED
  | LBRA
  | RBRA
  | AND
  | OR
  | NOT
  | EQUAL
  | GREATER
  | LESS
  | EOF
  | VAR of (string)
  | TRUE of (string)
  | FALSE of (string)
  | NUM of (int)
type tokenId = 
    | TOKEN_TIMES
    | TOKEN_DIV
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_POW
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_ASSIGN
    | TOKEN_SKIP
    | TOKEN_SEMICOLON
    | TOKEN_IF
    | TOKEN_FI
    | TOKEN_DO
    | TOKEN_OD
    | TOKEN_PRED
    | TOKEN_LBRA
    | TOKEN_RBRA
    | TOKEN_AND
    | TOKEN_OR
    | TOKEN_NOT
    | TOKEN_EQUAL
    | TOKEN_GREATER
    | TOKEN_LESS
    | TOKEN_EOF
    | TOKEN_VAR
    | TOKEN_TRUE
    | TOKEN_FALSE
    | TOKEN_NUM
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_expression
    | NONTERM_factor
    | NONTERM_power
    | NONTERM_unary
    | NONTERM_num
    | NONTERM_orbool
    | NONTERM_andbool
    | NONTERM_notbool
    | NONTERM_basebool
    | NONTERM_guardedcommand
    | NONTERM_command
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val start : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (command) 
