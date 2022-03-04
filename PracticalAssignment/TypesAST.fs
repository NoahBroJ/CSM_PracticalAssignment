// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module TypesAST

// C ::= x := a | A[a] := a | skip | C ; C | if GC fi | do GC od
// GC ::= b -> C | GC [] GC
// a ::= n | x | A[a] | a + a | a - a | a * a | a / a | - a | a ^ a | (a)
// b ::= true | false | b & b | b | b | b && b | b || b | !b
//			  | a = a | a != a | a > a | a >= a | a < a | a <= a | (b)


// type Arr =
//    | 

type aexpr =
    | Num of int
    | Var of string
    //| ArrayIndex of (Arr * aexpr)
    | ArrayIndex of (string * aexpr)
    | Plus of (aexpr * aexpr)
    | Minus of (aexpr * aexpr)
    | Times of (aexpr * aexpr)
    | Div of (aexpr * aexpr)
    | UMinus of (aexpr)
    | Pow of (aexpr * aexpr)

type bexpr = 
    | T of (bool)
    | F of (bool)
    | And1 of (bool * bool)
    | Or1 of (bool * bool)
    | And2 of (bool * bool)
    | Or2 of (bool * bool)
    | NEG of (bool)
    | EQ of (aexpr * aexpr)
    | NEQ of (aexpr * aexpr)
    | GT of (aexpr * aexpr)
    | GEQ of (aexpr * aexpr)
    | LT of (aexpr * aexpr)
    | LEQ of (aexpr * aexpr)
    
type guardedCommand = 
    | Pred of (bexpr * command)
    | Choice of (guardedCommand * guardedCommand)
and command = 
   | Assign of (string * aexpr)
   | ArrAssign of (string * aexpr * aexpr)
   | Skip
   | SemiColon of (command * command)
   | Iffi of (guardedCommand)
   | Dood of (guardedCommand)