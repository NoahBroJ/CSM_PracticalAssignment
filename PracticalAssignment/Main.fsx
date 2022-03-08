#r "10.0.0/lib/net46/FsLexYacc.Runtime.dll"

open FSharp.Text.Lexing
open System
#load "TypesAST.fs"
open TypesAST
#load "Parser.fs"
open Parser
#load "Lexer.fs"
open Lexer

let rec tab = function
    | 0 -> ""
    | n -> "    " + tab(n-1)


let rec evalC e n =
    match e with
    | Assign(x,y) -> tab(n) + evalA(x) + " := " + evalA(y)
    | ArrAssign(x,y,z) -> tab(n) + " " + evalA(x) + "[" + evalA(y) + "]" + " = " + evalA(z)
    | Skip -> tab(n) + "Skip"
    | SemiColon(x,y) -> evalC x n + "; \n" + evalC y n
    | Iffi(x) -> tab(n) + "if " + evalGC x (n+1) + "\nfi"
    | Dood(x) -> tab(n) + "do " + evalGC x (n+1) + "\nod"
and evalA e =
  match e with
    | Num(x) -> string x
    | Var(x) -> x
    | APar(x) -> "(" + evalA(x) + ")"
    | ArrayIndex(x,y) -> x + "[" + evalA(y) + "]"
    | Times(x,y) -> evalA(x) + " * " + evalA(y)
    | Div(x,y) -> evalA(x) + " / " + evalA(y)
    | Plus(x,y) -> evalA(x) + " + " + evalA(y)
    | Minus(x,y) -> evalA(x) + " - " + evalA(y)
    | Pow(x,y) -> evalA(x) + "^" + evalA(y)
    | UMinus(x) -> "-" + evalA(x)
and evalB e = 
    match e with
    | T -> "true"
    | F -> "false"
    | BPar(x) -> "(" + evalB(x) + ")"
    | And1(x,y) -> evalB(x) + " & " + evalB(y)
    | Or1(x,y) -> evalB(x) + " | " + evalB(y)
    | And2(x,y) -> evalB(x) + " && " + evalB(y)
    | Or2(x,y) -> evalB(x) + " || " + evalB(y)
    | NEG(x) -> "!" + evalB(x)
    | EQ(x,y) -> evalA(x) + " = " + evalA(y)
    | NEQ(x,y) -> evalA(x) + " != " + evalA(y)
    | GT(x,y) -> evalA(x) + " > " + evalA(y)
    | GEQ(x,y) -> evalA(x) + " >= " + evalA(y)
    | LT(x,y) -> evalA(x) + " < " + evalA(y)
    | LEQ(x,y) -> evalA(x) + " <= " + evalA(y)
and evalGC e n =
    match e with
    | Pred(x,y) -> evalB(x) + " -> \n" + evalC y n
    | Choice(x,y) -> tab(n) + evalGC x n + "\n[]\n" + evalGC y n

// We
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = Parser.start Lexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

// We implement here the function that interacts with the user
let rec compute =
        (* printf "Enter a GCL program: "
        try
        // We parse the input string
        let e = parse (Console.ReadLine())
        // and print the result of evaluating it *)
        let program = "i:=1;do i<n -> j:=i;do (j>0)&&(A[j-1]>A[j]) -> t:=A[j];A[j]:=A[j-1];A[j-1]:=t;j:=j-1 od;i:=i+1 od"
        printfn "Result: \n%s" (evalC(parse program) 0)


// Start interacting with the user
compute