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
let mutable det = false
(*

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
*)

let edgeMap = Map.empty<string, ((Map<string, int32> * Map<string, int[]>) -> (string * Map<string, int32> * Map<string, int[]>))>

let stupidOr b1 b2 = if not b1 then b2 else if b2 then true else false

let stupidAnd b1 b2 = if b1 then b2 else if b2 then false else false 

let rec insertAt x ys n =
    match n, ys with 
    | 1, _      
    | _, []     -> x::ys
    | _, y::ys  -> y::insertAt x ys (n-1)

let rec interpretC e qs qe edgeMap =
    match e with
    | Assign(x,y) -> Map.add qs (fun (v, a) -> (qe, (Map.add x (interpretA y v a) v), a)) edgeMap
    | ArrAssign(x,y,z) -> Map.add qs (fun (v, a) -> (qe, v, (Map.add x (insertAt (interpretA z v a) (Map.find x a)(interpretA y v a)) a))) edgeMap
    | Skip ->  Map.add qs (fun (v,a) -> (qe, v, a)) edgeMap
    | SemiColon(x,y) -> let qi = "q" + string fresh
                        fresh <- fresh + 1
                        let tempMap = interpretC x qs qi edgeMap
                        interpretC y qi qe tempMap
    | Iffi(x) -> interpretGC x qs qe edgeMap
    | Dood(x) -> let tempMap = (interpretGC x qs qs edgeMap)
                 let extraEdge = fun (v,a) -> if (interpretB (interpretOther x) v a) then (qe, v, a) else ((Map.find qs tempMap) (v,a))
                 Map.add qs extraEdge tempMap
and interpretA e varMap arrMap =
    match e with
    | Num(x) -> x
    | Var(x) -> Map.find x varMap
    | APar(x) -> interpretA x varMap arrMap
    | ArrayIndex(x,y) -> List.item (interpretA y varMap arrMap) (Map.find x arrMap)
    | Times(x,y) -> (interpretA x varMap arrMap) * (interpretA y varMap arrMap)
    | Div(x,y) -> (interpretA x varMap arrMap) / (interpretA y varMap arrMap)
    | Plus(x,y) -> (interpretA x varMap arrMap) + (interpretA y varMap arrMap)
    | Minus(x,y) -> (interpretA x varMap arrMap) - (interpretA y varMap arrMap)
    | Pow(x,y) -> pown (interpretA x varMap arrMap) (interpretA y varMap arrMap)
    | UMinus(x) -> 0 - (interpretA x varMap arrMap)
and interpretB e varMap arrMap = 
    match e with
    | T -> true
    | F -> false
    | BPar(x) -> interpretB x varMap arrMap
    | And1(x,y) -> stupidAnd (interpretB x varMap arrMap) (interpretB y varMap arrMap)
    | Or1(x,y) -> stupidOr (interpretB x varMap arrMap) (interpretB y varMap arrMap)
    | And2(x,y) -> (interpretB x varMap arrMap) && (interpretB y varMap arrMap)
    | Or2(x,y) -> (interpretB x varMap arrMap) || (interpretB y varMap arrMap)
    | NEG(x) -> not (interpretB x varMap arrMap)
    | EQ(x,y) -> (interpretA x varMap arrMap) = (interpretA y varMap arrMap)
    | NEQ(x,y) -> (interpretA x varMap arrMap) <> (interpretA y varMap arrMap)
    | GT(x,y) -> (interpretA x varMap arrMap) > (interpretA y varMap arrMap)
    | GEQ(x,y) -> (interpretA x varMap arrMap) >= (interpretA y varMap arrMap)
    | LT(x,y) -> (interpretA x varMap arrMap) < (interpretA y varMap arrMap)
    | LEQ(x,y) -> (interpretA x varMap arrMap) <= (interpretA y varMap arrMap)
and interpretGC e qs qe edgeMap =
    match e with
    | Pred(x,y) -> let qi = "q" + string fresh
                   fresh <- fresh + 1
                   if (Map.containsKey qs edgeMap) then
                       let tempMap = Map.add qs (fun (v, a) -> if (interpretB x v a) then (qi, v, a) else (Map.find qs edgeMap) (v, a)) edgeMap
                       interpretC y qi qe tempMap
                   else
                       let tempMap = Map.add qs (fun (v, a) -> if (interpretB x v a) then (qi, v, a) else (qs, v, a)) edgeMap
                       interpretC y qi qe tempMap
    | Choice(x,y) -> let tempMap = interpretGC x qs qe edgeMap 
                     interpretGC y qs qe tempMap
//and dinterpretGC e qs qe =
//    match e with
//    | Pred(x,y) -> let qi = "q" + string fresh
//                   fresh <- fresh + 1
//                   let dexp = d
//                   let res = qs + " -> " + qi + " [label = \"" + interpretB(And1(x, NEG(d))) + "\"];\n" + interpretC y qi qe
//                   d <- Or1(x, dexp)
//                   res
//    | Choice(x,y) -> dinterpretGC x qs qe + dinterpretGC y qs qe

and interpretOther gc =
    match gc with
    | Pred(x,y) -> NEG(x)
    | Choice(x,y) -> And2((interpretOther x), (interpretOther y))

//and dinterpretOther gc =
//    match gc with
//    | Pred(x,y) -> interpretB(NEG(d))
//    | Choice(x,y) -> dinterpretOther x + "&&" + dinterpretOther y

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
do (n>i)&&(A[i]>=0) -> x:=x+A[i];
                       y:=y+1;
                       i:=i+1
[] (n>i)&&(0>A[i]) -> i:=i+1
od;
x:=x/y"

// We implement here the function that interacts with the user
//let rec compute =
//        (* printf "Enter a GCL program: "
//        try
//        // We parse the input string
//        let e = parse (Console.ReadLine())
//        // and print the result of evaluating it *)
        
//        printfn "Result: \n%s" (evalC(parse program) 0)

(*
let rec compile determinstic =
    det <- determinstic
    printfn "Result: \n%s" (compileC(parse program) "qs" "qe")
*)

let rec run endNode node edgeMap varMap arrMap =
    printfn "---- %s ----" node
    for entry in (varMap:Map<string, int>) do
        printfn "%s = %d" entry.Key entry.Value
    printfn ""

    let (next, v, a) = (Map.find node edgeMap) (varMap, arrMap)

    if (next = endNode) then (v, a)
    else run endNode next edgeMap v a

let rec interpret deterministic =
    det <- deterministic
    let varMap = Map.ofList [("i", 0); ("n", 4); ("y", 0); ("x", 0)]
    let arrMap = Map.ofList [("A", [1;3;8;4])]
    let inputEdges = Map.ofList []
    let outputEdges = interpretC(parse program) "qs" "qe" inputEdges
    let (v, a) = run "qe" "qs" outputEdges varMap arrMap

    printfn "---- qe ----"
    for entry in (v:Map<string, int>) do
        printfn "%s = %d" entry.Key entry.Value
    printfn "STATUS: TERMINATED"

// Start interacting with the user
interpret false