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


//let rec evalC e n =
//    match e with
//    | Assign(x,y) -> tab(n) + evalA(x) + " := " + evalA(y)
//    | ArrAssign(x,y,z) -> tab(n) + " " + evalA(x) + "[" + evalA(y) + "]" + " = " + evalA(z)
//    | Skip -> tab(n) + "Skip"
//    | SemiColon(x,y) -> evalC x n + "; \n" + evalC y n
//    | Iffi(x) -> tab(n) + "if " + evalGC x (n+1) + "\nfi"
//    | Dood(x) -> tab(n) + "do " + evalGC x (n+1) + "\nod"
//and evalA e =
//  match e with
//    | Num(x) -> string x
//    | Var(x) -> x
//    | APar(x) -> "(" + evalA(x) + ")"
//    | ArrayIndex(x,y) -> x + "[" + evalA(y) + "]"
//    | Times(x,y) -> evalA(x) + " * " + evalA(y)
//    | Div(x,y) -> evalA(x) + " / " + evalA(y)
//    | Plus(x,y) -> evalA(x) + " + " + evalA(y)
//    | Minus(x,y) -> evalA(x) + " - " + evalA(y)
//    | Pow(x,y) -> evalA(x) + "^" + evalA(y)
//    | UMinus(x) -> "-" + evalA(x)
//and evalB e = 
//    match e with
//    | T -> "true"
//    | F -> "false"
//    | BPar(x) -> "(" + evalB(x) + ")"
//    | And1(x,y) -> evalB(x) + " & " + evalB(y)
//    | Or1(x,y) -> evalB(x) + " | " + evalB(y)
//    | And2(x,y) -> evalB(x) + " && " + evalB(y)
//    | Or2(x,y) -> evalB(x) + " || " + evalB(y)
//    | NEG(x) -> "!" + evalB(x)
//    | EQ(x,y) -> evalA(x) + " = " + evalA(y)
//    | NEQ(x,y) -> evalA(x) + " != " + evalA(y)
//    | GT(x,y) -> evalA(x) + " > " + evalA(y)
//    | GEQ(x,y) -> evalA(x) + " >= " + evalA(y)
//    | LT(x,y) -> evalA(x) + " < " + evalA(y)
//    | LEQ(x,y) -> evalA(x) + " <= " + evalA(y)
//and evalGC e n =
//    match e with
//    | Pred(x,y) -> evalB(x) + " -> \n" + evalC y n
//    | Choice(x,y) -> tab(n) + evalGC x n + "\n[]\n" + evalGC y n
    

let mutable fresh = 1
let mutable d = F
let mutable det = true

let rec compileC e qs qe =
    match e with
    | Assign(x,y) -> qs + " -> " + qe + " [label = \"" + x + ":=" + compileA(y) + "\"];\n"
    | ArrAssign(x,y,z) -> qs + " -> " + qe + " [label = \"" + x + "[" + compileA(y) + "]" + "=" + compileA(z) + "\"];\n"
    | Skip ->  qs + " -> " + qe + " [label = \"" + "Skip" + "\"];\n"
    | SemiColon(x,y) -> let qi = "q" + string fresh
                        fresh <- fresh + 1
                        compileC x qs qi + compileC y qi qe
    | Iffi(x) -> d <- F
                 if det then dcompileGC x qs qe else compileGC x qs qe
    | Dood(x) -> d <- F
                 if det then dcompileGC x qs qs + qs + " -> " + qe + " [label = \"" + dcompileOther x + "\"];\n" else compileGC x qs qs + qs + " -> " + qe + " [label = \"" + compileOther x + "\"];\n"
and compileA e =
    match e with
    | Num(x) -> string x
    | Var(x) -> x
    | APar(x) -> "(" + compileA(x) + ")"
    | ArrayIndex(x,y) -> x + "[" + compileA(y) + "]"
    | Times(x,y) -> compileA(x) + " * " + compileA(y)
    | Div(x,y) -> compileA(x) + " / " + compileA(y)
    | Plus(x,y) -> compileA(x) + " + " + compileA(y)
    | Minus(x,y) -> compileA(x) + " - " + compileA(y)
    | Pow(x,y) -> compileA(x) + "^" + compileA(y)
    | UMinus(x) -> "-" + compileA(x)
and compileB e = 
    match e with
    | T -> "true"
    | F -> "false"
    | BPar(x) -> "(" + compileB(x) + ")"
    | And1(x,y) -> compileB(x) + " & " + compileB(y)
    | Or1(x,y) -> compileB(x) + " | " + compileB(y)
    | And2(x,y) -> compileB(x) + " && " + compileB(y)
    | Or2(x,y) -> compileB(x) + " || " + compileB(y)
    | NEG(x) -> "!(" + compileB(x) + ")"
    | EQ(x,y) -> compileA(x) + " = " + compileA(y)
    | NEQ(x,y) -> compileA(x) + " != " + compileA(y)
    | GT(x,y) -> compileA(x) + " > " + compileA(y)
    | GEQ(x,y) -> compileA(x) + " >= " + compileA(y)
    | LT(x,y) -> compileA(x) + " < " + compileA(y)
    | LEQ(x,y) -> compileA(x) + " <= " + compileA(y)
and compileGC e qs qe =
    match e with
    | Pred(x,y) -> let qi = "q" + string fresh
                   fresh <- fresh + 1
                   qs + " -> " + qi + " [label = \"" + compileB(x) + "\"];\n" + compileC y qi qe
    | Choice(x,y) -> compileGC x qs qe + compileGC y qs qe
and dcompileGC e qs qe =
    match e with
    | Pred(x,y) -> let qi = "q" + string fresh
                   fresh <- fresh + 1
                   let dexp = d
                   let res = qs + " -> " + qi + " [label = \"" + compileB(And1(x, NEG(d))) + "\"];\n" + compileC y qi qe
                   d <- Or1(x, dexp)
                   res
    | Choice(x,y) -> dcompileGC x qs qe + dcompileGC y qs qe

and compileOther gc =
    match gc with
    | Pred(x,y) -> compileB(NEG(x))
    | Choice(x,y) -> compileOther x + "&&" + compileOther y

and dcompileOther gc =
    match gc with
    | Pred(x,y) -> compileB(NEG(d))
    | Choice(x,y) -> dcompileOther x + "&&" + dcompileOther y

// We
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = Parser.start Lexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

let program = "i:=0;
x:=0;
y:=0;
do i<10 -> if A[i]>=0 -> x:=x+A[i];
                         i:=i+1
           [] A[i]<0 -> i:=i+1;
                        i:=i+2
           fi;
           y:=y+1
od"

// We implement here the function that interacts with the user
//let rec compute =
//        (* printf "Enter a GCL program: "
//        try
//        // We parse the input string
//        let e = parse (Console.ReadLine())
//        // and print the result of evaluating it *)
        
//        printfn "Result: \n%s" (evalC(parse program) 0)

let rec compile determinstic =
    det <- determinstic
    printfn "Result: \n%s" (compileC(parse program) "qs" "qe")


// Start interacting with the user
compile true