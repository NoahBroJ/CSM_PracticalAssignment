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

let stupidOr b1 b2 = if not b1 then b2 else if b2 then true else false

let stupidAnd b1 b2 = if b1 then b2 else if b2 then false else false 

let rec insertAt x ys n =
    match n, ys with
    | 0, _::ys -> x::ys
    | n, y::ys -> y::(insertAt x ys (n - 1))
    | _, [] -> raise (System.ArgumentException("The fuck you doin"))

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
                       let tempMap = Map.add qs (fun (v, a) -> if (interpretB x v a) then (qi, v, a) else ("stuck", v, a)) edgeMap
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

type action =
    | ACommand of command
    | ABexpr of bexpr

type path =
    | PCommand of (string * action)
    | PSplit of (List<(string * action)> * bool)


let rec buildC e qs qe edgeMap =
    match e with
    | Assign(x,y) -> Map.add qs (PCommand(qe, (ACommand(Assign(x,y))))) edgeMap
    | ArrAssign(x,y,z) -> Map.add qs (PCommand(qe, (ACommand(ArrAssign(x,y,z))))) edgeMap
    | Skip ->  Map.add qs (PCommand(qe, ACommand(Skip))) edgeMap
    | SemiColon(x,y) -> let qi = "q" + string fresh
                        fresh <- fresh + 1
                        let tempMap = buildC x qs qi edgeMap
                        buildC y qi qe tempMap
    | Iffi(x) -> buildGC x qs qe edgeMap
    | Dood(x) -> let tempMap = (buildGC x qs qs edgeMap)
                 match Map.find qs tempMap with
                 | PSplit(splitList, _) -> let extraSplit = PSplit(((qe, (ABexpr(buildOther x)))::splitList), true)
                                           Map.add qs extraSplit tempMap
and buildGC e qs qe edgeMap=
   match e with
   | Pred(x,y) -> let qi = "q" + string fresh
                  fresh <- fresh + 1
                  if (Map.containsKey qs edgeMap) then
                      match Map.find qs edgeMap with
                      | PSplit(splitList, isDo) -> let tempMap = Map.add qs (PSplit(((qi, ABexpr(x))::splitList), isDo)) edgeMap
                                                   buildC y qi qe tempMap
                  else
                      let tempMap = Map.add qs (PSplit(((qi, ABexpr(x))::[]), false)) edgeMap
                      buildC y qi qe tempMap
   | Choice(x,y) -> let tempMap = buildGC x qs qe edgeMap 
                    buildGC y qs qe tempMap
and buildOther gc =
    match gc with
    | Pred(x,y) -> NEG(x)
    | Choice(x,y) -> And2((buildOther x), (buildOther y))
        

// We
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = Parser.start Lexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

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

    for entry in (arrMap:Map<string, List<int>>) do
        printf "%s = [" entry.Key
        match entry.Value with
        | v::vs -> printf "%d" v
                   for number in (vs) do
                       printf ", %d" number
        | [] -> printf ""
        printfn "]"

    printfn ""

    let (next, v, a) = 
        try 
            (Map.find node edgeMap) (varMap, arrMap)
        with
        | :? _ -> ("stuck", varMap, arrMap)

    if (next = endNode) then (next, v, a, "TERMINATED")
    elif (next = "stuck") then (next, v, a, "STUCK")
    else run endNode next edgeMap v a

//let rec interpret deterministic =
//    det <- deterministic
//    let varMap = Map.ofList [("i", 0); ("j", 0); ("n", 5); ("t", 0)]
//    let arrMap = Map.ofList [("A", [2])]
//    let inputEdges = Map.ofList []
//    let outputEdges = interpretC(parse program) "qs" "qe" inputEdges
//    let (endNode, v, a, s) = run "qe" "qs" outputEdges varMap arrMap

//    printfn "---- %s ----" endNode
//    for entry in (v:Map<string, int>) do
//        printfn "%s = %d" entry.Key entry.Value
    
//    for entry in (a:Map<string, List<int>>) do
//        printf "%s = [" entry.Key
//        match entry.Value with
//        | v::vs -> printf "%d" v
//                   for number in (vs) do
//                       printf ", %d" number
//        | [] -> printf ""
//        printfn "]"

//    printfn "STATUS: %s" s

let getDomP pg =
    let isDoNode = function
        | PSplit(_, isDo) -> isDo
        | _ -> false

    Map.fold (fun domP q path -> if (isDoNode path) then (domP@[q]) else domP) [] pg

let getPointingEdges q pg =
    let isPointingEdge = function
        | PCommand(pointingTo, command) when pointingTo = q -> Some(command)
        | PSplit(pathList, _) -> match List.tryFind (fun (pointingTo, _) -> pointingTo = q) pathList with
                                | Some(_, bexpr) -> Some(bexpr)
                                | _ -> None
        | _ -> None

    let addToPointingEdges pointingEdges pointingFrom path =
        match isPointingEdge path with
        | Some(shortpath) -> (pointingFrom, shortpath)::pointingEdges
        | _ -> pointingEdges

    // fold: (’a -> ’b -> ’c -> ’a) -> ’a -> Map<’b, ’c> -> ’a
    // fold: (List<(string, shortPath)> -> string -> path -> List<(string, shortPath)>) -> List<(string, shortPath)> -> Map<string, path> -> List<(string, shortPath)>
    Map.fold addToPointingEdges [] pg

let rec getSPFs qs edges qe pg domP =
    List.fold (fun fragmentSet (q, shortpath) -> if (List.contains q domP) then (q, (shortpath::edges), qe)::fragmentSet else (getSPFs q (shortpath::edges) qe pg domP)@fragmentSet) [] (getPointingEdges qs pg)

let replaceVar x (action:action) =
    match action with
    | ACommand(c) -> match c with
                    | Assign(s, a) when s = x -> a
                    | _ -> Var(x)
    | _ -> Var(x)

let rec exprAreEqual expr1 expr2 =
    match (expr1, expr2) with
    | ((Num(n1)), (Num(n2))) -> n1 = n2
    | ((Var(x1)), (Var(x2))) -> x1 = x2
    | ((APar(a1)), (APar(a2))) -> exprAreEqual a1 a2
    | ((ArrayIndex(x1 ,a1)), (ArrayIndex(x2, a2))) -> (x1 = x2) && (exprAreEqual a1 a2)
    | ((Plus(a11, a12)), (Plus(a21, a22))) -> ((exprAreEqual a11 a21) && (exprAreEqual a12 a22)) || ((exprAreEqual a11 a22) && (exprAreEqual a12 a21))
    | ((Minus(a11, a12)), (Minus(a21, a22))) -> (exprAreEqual a11 a21) && (exprAreEqual a12 a22)
    | ((Times(a11, a12)), (Times(a21, a22))) -> ((exprAreEqual a11 a21) && (exprAreEqual a12 a22)) || ((exprAreEqual a11 a22) && (exprAreEqual a12 a21))
    | ((Div(a11, a12)), (Div(a21, a22))) -> (exprAreEqual a11 a21) && (exprAreEqual a12 a22)
    | ((UMinus(a1)), (UMinus(a2))) -> exprAreEqual a1 a2
    | ((Pow(a11, a12)), (Pow(a21, a22))) -> (exprAreEqual a11 a21) && (exprAreEqual a12 a22)
    | _ -> false

let replaceArr A i (action:action) =
    match action with
    | ACommand(c) -> match c with
                    | ArrAssign(s, index, a) when (s = A) && (exprAreEqual index i) -> a
                    | _ -> ArrayIndex(A, i)
    | _ -> ArrayIndex(A, i)

let rec transformArithmetic (expr:aexpr) (action:action) =
    match expr with
    | Num(n) -> Num(n)
    | Var(s) -> replaceVar s action
    | APar(a) -> APar(transformArithmetic a action)
    | ArrayIndex(s, a) -> replaceArr s a action
    | Plus(a1, a2) -> Plus((transformArithmetic a1 action), (transformArithmetic a2 action))
    | Minus(a1, a2) -> Minus((transformArithmetic a1 action), (transformArithmetic a2 action))
    | Times(a1, a2) -> Times((transformArithmetic a1 action), (transformArithmetic a2 action))
    | Div(a1, a2) -> Div((transformArithmetic a1 action), (transformArithmetic a2 action))
    | UMinus(a) -> UMinus(transformArithmetic a action)
    | Pow(a1, a2) -> Pow((transformArithmetic a1 action), (transformArithmetic a2 action))

let rec transformPredicate (predicate:pexpr) (action:action) =
    match predicate with
    | PT -> PT
    | POr(p1, p2) -> POr((transformPredicate p1 action), (transformPredicate p2 action))
    | PAnd(p1, p2) -> PAnd((transformPredicate p1 action), (transformPredicate p2 action))
    | PNEG(p) -> PNEG((transformPredicate p action))
    | PEQ(a1, a2) -> PEQ((transformArithmetic a1 action), (transformArithmetic a2 action))
    | PNEQ(a1, a2) -> PNEQ((transformArithmetic a1 action), (transformArithmetic a2 action))
    | PGT(a1, a2) -> PGT((transformArithmetic a1 action), (transformArithmetic a2 action))
    | PGEQ(a1, a2) -> PGEQ((transformArithmetic a1 action), (transformArithmetic a2 action))
    | PLT(a1, a2) -> PLT((transformArithmetic a1 action), (transformArithmetic a2 action))
    | PLEQ(a1, a2) -> PLEQ((transformArithmetic a1 action), (transformArithmetic a2 action))

let rec aexprToString e =
    match e with
    | Num(x) -> string x
    | Var(x) -> x
    | APar(x) -> "(" + aexprToString(x) + ")"
    | ArrayIndex(x,y) -> "(" + x + "[" + aexprToString(y) + "]" + ")"
    | Times(x,y) -> "(" + aexprToString(x) + " * " + aexprToString(y) + ")"
    | Div(x,y) -> "(" + aexprToString(x) + " / " + aexprToString(y) + ")"
    | Plus(x,y) -> "(" + aexprToString(x) + " + " + aexprToString(y) + ")"
    | Minus(x,y) -> "(" + aexprToString(x) + " - " + aexprToString(y) + ")"
    | Pow(x,y) -> "(" + aexprToString(x) + "^" + aexprToString(y) + ")"
    | UMinus(x) -> "(" + "-" + aexprToString(x) + ")"
and pexprToString e = 
    match e with
    | PT -> "true"
    | PAnd(x,y) -> "(" + pexprToString(x) + " && " + pexprToString(y) + ")"
    | POr(x,y) -> "(" + pexprToString(x) + " || " + pexprToString(y) + ")"
    | PNEG(x) -> "!(" + pexprToString(x) + ")"
    | PEQ(x,y) -> "(" + aexprToString(x) + " = " + aexprToString(y) + ")"
    | PNEQ(x,y) -> "(" + aexprToString(x) + " != " + aexprToString(y) + ")"
    | PGT(x,y) -> "(" + aexprToString(x) + " > " + aexprToString(y) + ")"
    | PGEQ(x,y) -> "(" + aexprToString(x) + " >= " + aexprToString(y) + ")"
    | PLT(x,y) -> "(" + aexprToString(x) + " < " + aexprToString(y) + ")"
    | PLEQ(x,y) -> "(" + aexprToString(x) + " <= " + aexprToString(y) + ")"

let getProofObligation(predS, actionList, predE) =
    //(’a -> ’b -> ’a) -> ’a -> ’b list -> ’a
    //(pexpr -> shortPath -> pexpr) -> pexpr -> shortPath list -> pexpr
    let predT = List.fold transformPredicate predE actionList
    (pexprToString predS) + " => " + (pexprToString predT)



let rec verify program startPhi endPhi loopPhis =
    let startNode = "qs"
    let endNode = "qe"
    let pg = buildC(parse program) startNode endNode (Map.ofList [])
    // Get Dom(P) DONE
    let domP = [startNode]@(getDomP pg)@[endNode]
    // Get SPFs DONE
    //(’a -> ’b -> ’a) -> ’a -> ’b list -> ’a
    //((string * shortPath list * string) list -> string -> (string * shortPath list * string) list) -> (string * shortPath list * string) list -> string list -> (string * shortPath list * string) list
    let spfs = List.fold (fun spfList node -> spfList@(getSPFs node [] node pg domP)) [] domP
    // Get user-defined predicates for each node in Dom(P) (precondition, postcondition, loop invariants)
    // For each SPF, check that the postcondition transformed by the path fragment is a tautology of the precondition
    let mapNodeToPredicate node =
        if (node = startNode) then
            startPhi
        elif (node = endNode) then
            endPhi
        else
            let index = List.findIndex (fun n -> n = node) domP
            List.item (index - 1) loopPhis

    let predicatePathFragments = List.map (fun (sNode, pathList, eNode) -> (mapNodeToPredicate sNode, pathList, mapNodeToPredicate eNode)) spfs
    let proofObligations = List.map getProofObligation predicatePathFragments

    for obligation in proofObligations do
        printfn "%s" obligation

// Start interacting with the user
let program = "z:=0;
do y>0 -> z:=z+x;
y:=y-1
od"

let startPhi = PAnd(PEQ(Var("x"), Num(3)), PEQ(Var("y"), Num(2)))
let endPhi = PEQ(Var("z"), Num(6))
let loopPhis = [PEQ(Var("z"), Times(Var("x"), Minus(Num(2), Var("y"))))]

verify program startPhi endPhi loopPhis