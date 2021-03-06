#r "10.0.0/lib/net46/FsLexYacc.Runtime.dll"

open FSharp.Text.Lexing
open System
#load "TypesAST.fs"
open TypesAST
#load "Parser.fs"
open Parser
#load "Lexer.fs"
open Lexer

let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = Parser.start Lexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res
    
let mutable fresh = 1
    
let rec edgesC((qStart:string), (qEnd:string), (command:command)) : List<(edge)> =
    match command with
    | Assign(x, a) ->       [(qStart, (ActAssign(x,a)), qEnd)]
    | ArrAssign(A, i, a) -> [(qStart, (ActArrAssign(A,i,a)), qEnd)]
    | Skip ->               [(qStart, ActSkip, qEnd)]
    | SemiColon(c1, c2) ->  let qi = "q" + string fresh
                            fresh <- fresh + 1
                            (edgesC(qStart, qi, c1))@(edgesC(qi, qEnd, c2))
    | Iffi(gc) ->           edgesGC(qStart, qEnd, gc)
    | Dood(gc) ->           let b = doneGC(gc)
                            (edgesGC(qStart, qStart, gc))@[(qStart, (ActCheck(b)), qEnd)]
    
and edgesGC((qStart:string), (qEnd:string), (gCommand:guardedCommand)) : List<(edge)> =
    match gCommand with
    | Pred(b, c) ->       let qi = "q" + string fresh
                          fresh <- fresh + 1
                          (edgesC(qi, qEnd, c))@[(qStart, (ActCheck(b)), qi)]
    | Choice(gc1, gc2) -> (edgesGC(qStart, qEnd, gc1))@(edgesGC(qStart, qEnd, gc2))
    
and doneGC (gCommand:guardedCommand) : bexpr =
    match gCommand with
    | Pred(b, c) ->       NEG(b)
    | Choice(gc1, gc2) -> And2((doneGC(gc1)), (doneGC(gc2)))
    
let rec edgesDC((qStart:string), (qEnd:string), (command:command)) : List<(edge)> =
    match command with
    | Assign(x, a) ->       [(qStart, (ActAssign(x,a)), qEnd)]
    | ArrAssign(A, i, a) -> [(qStart, (ActArrAssign(A,i,a)), qEnd)]
    | Skip ->               [(qStart, ActSkip, qEnd)]
    | SemiColon(c1, c2) ->  let qi = "q" + string fresh
                            fresh <- fresh + 1
                            (edgesDC(qStart, qi, c1))@(edgesDC(qi, qEnd, c2))
    | Iffi(gc) ->           let (E, d) = edgesDGC(qStart, qEnd, gc) F
                            E
    | Dood(gc) ->           let (E, d) = edgesDGC(qStart, qStart, gc) F
                            E@[(qStart, (ActCheck(NEG(d))), qEnd)]
    
and edgesDGC((qStart:string), (qEnd:string), (gCommand:guardedCommand)) d : (List<(edge)> * bexpr) =
    match gCommand with
    | Pred(b, c) ->       let qi = "q" + string fresh
                          fresh <- fresh + 1
                          (((edgesDC(qi, qEnd, c))@[(qStart, (ActCheck(And2(b, NEG(d)))), qi)]), (Or2(b, d)))
    | Choice(gc1, gc2) -> let (E1, d1) = edgesDGC(qStart, qEnd, gc1) d
                          let (E2, d2) = edgesDGC(qStart, qEnd, gc2) d1
                          ((E1@E2), d2)

//
// TASK 1
//
let rec tab = function
    | 0 -> ""
    | n -> "    " + tab(n-1)

let rec CToString e n =
    match e with
    | Assign(x, a) -> tab(n) + x + " := " + AToString(a)
    | ArrAssign(A,i,a) -> tab(n) + " " + A + "[" + AToString(i) + "]" + " = " + AToString(a)
    | Skip -> tab(n) + "Skip"
    | SemiColon(c1,c2) -> CToString c1 n + "; \n" + CToString c2 n
    | Iffi(gc) -> tab(n) + "if " + GCToString gc (n+1) + "\nfi"
    | Dood(gc) -> tab(n) + "do " + GCToString gc (n+1) + "\nod"
and AToString e =
    match e with
    | Num(n) -> string n
    | Var(x) -> x
    | APar(a) -> "(" + AToString(a) + ")"
    | ArrayIndex(A,i) -> A + "[" + AToString(i) + "]"
    | Times(a1,a2) -> AToString(a1) + " * " + AToString(a2)
    | Div(a1,a2) -> AToString(a1) + " / " + AToString(a2)
    | Plus(a1,a2) -> AToString(a1) + " + " + AToString(a2)
    | Minus(a1,a2) -> AToString(a1) + " - " + AToString(a2)
    | Pow(a1,a2) -> AToString(a1) + "^" + AToString(a2)
    | UMinus(a) -> "-" + AToString(a)
and BToString e = 
    match e with
    | T -> "true"
    | F -> "false"
    | BPar(b) -> "(" + BToString(b) + ")"
    | And1(b1,b2) -> BToString(b1) + " & " + BToString(b2)
    | Or1(b1,b2) -> BToString(b1) + " | " + BToString(b2)
    | And2(b1,b2) -> BToString(b1) + " && " + BToString(b2)
    | Or2(b1,b2) -> BToString(b1) + " || " + BToString(b2)
    | NEG(b) -> "!" + BToString(b)
    | EQ(a1,a2) -> AToString(a1) + " = " + AToString(a2)
    | NEQ(a1,a2) -> AToString(a1) + " != " + AToString(a2)
    | GT(a1,b2) -> AToString(a1) + " > " + AToString(b2)
    | GEQ(a1,a2) -> AToString(a1) + " >= " + AToString(a2)
    | LT(a1,a2) -> AToString(a1) + " < " + AToString(a2)
    | LEQ(a1,a2) -> AToString(a1) + " <= " + AToString(a2)
and GCToString e n =
    match e with
    | Pred(b,c) -> BToString(b) + " -> \n" + CToString c n
    | Choice(gc1,gc2) -> tab(n) + GCToString gc1 n + "\n[]\n" + GCToString gc2 n

let printAST program =
    let ast = parse program
    printf "%s" (CToString ast 0)

//
// TASK 2
//
let getLabel((qStart:string), (action:action), (qEnd:string)) : string =
    match action with
    | ActAssign(x, a) ->       qStart + " -> " + qEnd + " [label = \"" + x + " := " + AToString(a) + "\"];\n"
    | ActArrAssign(A, i, a) -> qStart + " -> " + qEnd + " [label = \"" + A + "[" + AToString(i) + "] := " + AToString(a) + "\"];\n"
    | ActSkip ->               qStart + " -> " + qEnd + " [label = \"Skip\"];\n"
    | ActCheck(b) ->           qStart + " -> " + qEnd + " [label = \"" + BToString(b) + "\"];\n"

let printLabels program =
    let programGraph = edgesDC("qs", "qe", (parse program))
    for edge in programGraph do
        printf "%s" (getLabel edge)

//
// TASK 3
//
let stupidOr b1 b2 = if not b1 then b2 else if b2 then true else false

let stupidAnd b1 b2 = if b1 then b2 else if b2 then false else false

let rec insertAt x i A =
    match i, A with
    | 0, _::xs -> x::xs
    | n, y::xs -> y::(insertAt x (n - 1) xs)
    | _, [] -> raise (System.ArgumentException("Index out of range"))

let rec evalMemA (a:aexpr) ((varMap:Map<string, int>), (arrMap:Map<string, List<int>>)) : int =
    match a with
    | Num(n) -> n
    | Var(x) -> Map.find x varMap
    | APar(a) -> evalMemA a (varMap, arrMap)
    | ArrayIndex(A,i) -> List.item (evalMemA i (varMap, arrMap)) (Map.find A arrMap)
    | Times(a1,a2) -> (evalMemA a1 (varMap, arrMap)) * (evalMemA a2 (varMap, arrMap))
    | Div(a1,a2) -> (evalMemA a1 (varMap, arrMap)) / (evalMemA a2 (varMap, arrMap))
    | Plus(a1,a2) -> (evalMemA a1 (varMap, arrMap)) + (evalMemA a2 (varMap, arrMap))
    | Minus(a1,a2) -> (evalMemA a1 (varMap, arrMap)) - (evalMemA a2 (varMap, arrMap))
    | Pow(a1,a2) -> pown (evalMemA a1 (varMap, arrMap)) (evalMemA a2 (varMap, arrMap))
    | UMinus(a) -> (-1) * (evalMemA a (varMap, arrMap))

let rec evalMemB (b:bexpr) ((varMap:Map<string, int>), (arrMap:Map<string, List<int>>)) : bool =
    match b with
    | T -> true
    | F -> false
    | BPar(b) -> evalMemB b (varMap, arrMap)
    | And1(b1,b2) -> stupidAnd (evalMemB b1 (varMap, arrMap)) (evalMemB b2 (varMap, arrMap))
    | Or1(b1,b2) -> stupidOr (evalMemB b1 (varMap, arrMap)) (evalMemB b2 (varMap, arrMap))
    | And2(b1,b2) -> (evalMemB b1 (varMap, arrMap)) && (evalMemB b2 (varMap, arrMap))
    | Or2(b1,b2) -> (evalMemB b1 (varMap, arrMap)) || (evalMemB b2 (varMap, arrMap))
    | NEG(b) -> not (evalMemB b (varMap, arrMap))
    | EQ(a1,a2) -> (evalMemA a1 (varMap, arrMap)) = (evalMemA a2 (varMap, arrMap))
    | NEQ(a1,a2) -> (evalMemA a1 (varMap, arrMap)) <> (evalMemA a2 (varMap, arrMap))
    | GT(a1,a2) -> (evalMemA a1 (varMap, arrMap)) > (evalMemA a2 (varMap, arrMap))
    | GEQ(a1,a2) -> (evalMemA a1 (varMap, arrMap)) >= (evalMemA a2 (varMap, arrMap))
    | LT(a1,a2) -> (evalMemA a1 (varMap, arrMap)) < (evalMemA a2 (varMap, arrMap))
    | LEQ(a1,a2) -> (evalMemA a1 (varMap, arrMap)) <= (evalMemA a2 (varMap, arrMap))

let rec evalMem((qStart:string), (action:action), (qEnd:string)) ((varMap:Map<string, int>), (arrMap:Map<string, List<int>>)) : (string * Map<string, int> * Map<string, List<int>>) =
    match action with
    | ActAssign(x, a) ->       if (Map.containsKey x varMap) then
                                   let newVarMap = Map.add x (evalMemA a (varMap, arrMap)) varMap
                                   (qEnd, newVarMap, arrMap)
                               else
                                   raise (System.ArgumentException("Assignment failed"))
    | ActArrAssign(A, i, a) -> if (Map.containsKey A arrMap) then
                                   let newArrMap = Map.add A (insertAt (evalMemA a (varMap, arrMap)) (evalMemA i (varMap, arrMap)) (Map.find A arrMap)) arrMap
                                   (qEnd, varMap, newArrMap)
                               else
                                   raise (System.ArgumentException("Assignment failed"))
    | ActSkip ->               (qEnd, varMap, arrMap)
    | ActCheck(b) ->           if (evalMemB b (varMap, arrMap)) then
                                   (qEnd, varMap, arrMap)
                               else
                                   raise (System.ArgumentException("Check failed"))

let rec run endNode currentNode programGraph varMap arrMap =
    printfn "---- %s ----" currentNode
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

    let rec evalEdges (varMap, arrMap) = function
        | [edge] ->                      evalMem edge (varMap, arrMap)
        | (qs, ActCheck(b), qe)::rest -> try evalMem (qs, ActCheck(b), qe) (varMap, arrMap)
                                         with
                                         | :? _ -> evalEdges (varMap, arrMap) rest
        | _ ->                           raise (System.Exception("No outgoing edges"))

    let (next, v, a) = 
        try let currentEdges = List.filter (fun (sNode, _, _) -> sNode = currentNode) programGraph
            evalEdges (varMap, arrMap) currentEdges
        with
        | :? _ -> ("stuck", varMap, arrMap)

    if (next = endNode) then (next, v, a, "TERMINATED")
    elif (next = "stuck") then (next, v, a, "STUCK")
    else run endNode next programGraph v a

let interpret program varMap arrMap =
    let programGraph = edgesC("qs", "qe", (parse program))
    let (endNode, v, a, s) = run "qe" "qs" programGraph varMap arrMap

    printfn "---- %s ----" endNode
    for entry in (v:Map<string, int>) do
        printfn "%s = %d" entry.Key entry.Value
    
    for entry in (a:Map<string, List<int>>) do
        printf "%s = [" entry.Key
        match entry.Value with
        | v::vs -> printf "%d" v
                   for number in (vs) do
                       printf ", %d" number
        | [] -> printf ""
        printfn "]"

    printfn "STATUS: %s" s

//
// TASK 4
//
let getPointingEdges node programGraph =
    let tryGetPointingEdge = function
        | (qs, action, qe) when qe = node -> Some(qs, action, qe)
        | _ ->                            None

    let addToPointingEdges pointingEdges edge =
        match tryGetPointingEdge edge with
        | Some(pointingEdge) -> (pointingEdge)::pointingEdges
        | _ -> pointingEdges

    // fold: (’a -> ’b -> ’a) -> ’a -> List<’b> -> ’a
    // fold: (List<(string, shortPath)> -> string -> path -> List<(string, shortPath)>) -> List<(string, shortPath)> -> Map<string, path> -> List<(string, shortPath)>
    List.fold addToPointingEdges [] programGraph

let isDoNode node programGraph =
    let pointingEdges = getPointingEdges node programGraph
    (node = "qs" && (not (List.isEmpty pointingEdges))) || (node <> "qs" && List.length pointingEdges > 1)

let getDomP programGraph =
    List.map (fun (qs, _, _) -> qs) (List.filter (fun (qs, _, _) -> isDoNode qs programGraph) programGraph)


let rec getSPFs qStart edges qEnd pg domP =
    List.fold (fun fragmentSet (qs, action, _) -> if (List.contains qs domP) then (qs, (action::edges), qEnd)::fragmentSet else (getSPFs qs (action::edges) qEnd pg domP)@fragmentSet) [] (getPointingEdges qStart pg)

let replaceVar varName (action:action) =
    match action with
    | ActAssign(x, a) when x = varName -> a
    | _ -> Var(varName)

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

let replaceArr arrName index (action:action) =
    match action with
    | ActArrAssign(A, i, a) when (A = arrName) && (exprAreEqual i index) -> a
    | _ -> ArrayIndex(arrName, index)

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
    let pg = edgesC(startNode, endNode, (parse program))
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

    let predicatePathFragments = List.map (fun (sNode, actionList, eNode) -> (mapNodeToPredicate sNode, actionList, mapNodeToPredicate eNode)) spfs
    let proofObligations = List.map getProofObligation predicatePathFragments

    for obligation in proofObligations do
        printfn "%s" obligation

//
// TASK 5
//
let rec cartesian setA setB f = 
    Set.fold (fun stateA signA -> Set.union stateA (Set.fold (fun stateB signB -> Set.union stateB (f signA signB)) Set.empty setB)) Set.empty setA

let flipSign = function
    | MINUSSIGN -> PLUSSIGN
    | PLUSSIGN -> MINUSSIGN
    | ZEROSIGN -> ZEROSIGN

let signPlus sign1 sign2 =
    match sign1, sign2 with
    | (MINUSSIGN, PLUSSIGN) -> Set.empty.Add(MINUSSIGN).Add(ZEROSIGN).Add(PLUSSIGN)
    | (MINUSSIGN, _) -> Set.empty.Add(MINUSSIGN)
    | (ZEROSIGN, sign) -> Set.empty.Add(sign)
    | (PLUSSIGN, MINUSSIGN) -> Set.empty.Add(MINUSSIGN).Add(ZEROSIGN).Add(PLUSSIGN)
    | (PLUSSIGN, _) -> Set.empty.Add(PLUSSIGN)
    | _ -> Set.empty
    
let signMinus sign1 sign2 =
    match sign1, sign2 with
    | (MINUSSIGN, MINUSSIGN) -> Set.empty.Add(MINUSSIGN).Add(ZEROSIGN).Add(PLUSSIGN)
    | (MINUSSIGN, _) -> Set.empty.Add(MINUSSIGN)
    | (ZEROSIGN, sign) -> Set.empty.Add(flipSign sign)
    | (PLUSSIGN, PLUSSIGN) -> Set.empty.Add(MINUSSIGN).Add(ZEROSIGN).Add(PLUSSIGN)
    | (PLUSSIGN, _) -> Set.empty.Add(PLUSSIGN)
    | _ -> Set.empty

let signTimes sign1 sign2 =
    match sign1, sign2 with
    | (ZEROSIGN, _) -> Set.empty.Add(ZEROSIGN)
    | (_, ZEROSIGN) -> Set.empty.Add(ZEROSIGN)
    | (MINUSSIGN, sign) -> Set.empty.Add(flipSign sign)
    | (sign, MINUSSIGN) -> Set.empty.Add(flipSign sign)
    | (PLUSSIGN, PLUSSIGN) -> Set.empty.Add(PLUSSIGN)
    | _ -> Set.empty
    
let signDiv sign1 sign2 =
    match sign1, sign2 with
    | (ZEROSIGN, _) -> Set.empty.Add(ZEROSIGN)
    | (_, ZEROSIGN) -> Set.empty // Throw error here? But where catch?
    | (MINUSSIGN, sign) -> Set.empty.Add(flipSign sign).Add(ZEROSIGN)
    | (sign, MINUSSIGN) -> Set.empty.Add(flipSign sign).Add(ZEROSIGN)
    | (PLUSSIGN, PLUSSIGN) -> Set.empty.Add(PLUSSIGN).Add(ZEROSIGN)
    | _ -> Set.empty
    
let signUnary sign = flipSign sign

let signPow sign1 sign2 =
    match sign1, sign2 with
    | (_, ZEROSIGN) -> Set.empty.Add(PLUSSIGN)
    | (ZEROSIGN, MINUSSIGN) -> Set.empty
    | (ZEROSIGN, PLUSSIGN) -> Set.empty.Add(ZEROSIGN)
    | (MINUSSIGN, MINUSSIGN) -> Set.empty.Add(MINUSSIGN).Add(ZEROSIGN).Add(PLUSSIGN)
    | (MINUSSIGN, PLUSSIGN) -> Set.empty.Add(MINUSSIGN).Add(PLUSSIGN)
    | (PLUSSIGN, MINUSSIGN) -> Set.empty.Add(PLUSSIGN).Add(ZEROSIGN)
    | (PLUSSIGN, PLUSSIGN) -> Set.empty.Add(PLUSSIGN)

let rec signEvalA a varSigns arrSigns =
    match a with
    | Num(n) -> if (n > 0) then Set.singleton(PLUSSIGN)
                elif (n < 0) then Set.singleton(MINUSSIGN)
                else Set.singleton(ZEROSIGN)
    | Var(x) -> Set.singleton(Map.find x varSigns)
    | APar(a) -> signEvalA a varSigns arrSigns
    | ArrayIndex(A, i) -> if (not (Set.isEmpty (Set.intersect (signEvalA i varSigns arrSigns) (set [ZEROSIGN; PLUSSIGN])))) then
                              Map.find A arrSigns
                          else
                              Set.empty
    | Plus(a1, a2) -> cartesian (signEvalA a1 varSigns arrSigns) (signEvalA a2 varSigns arrSigns) signPlus
    | Minus(a1, a2) -> cartesian (signEvalA a1 varSigns arrSigns) (signEvalA a2 varSigns arrSigns) signMinus
    | Times(a1, a2) -> cartesian (signEvalA a1 varSigns arrSigns) (signEvalA a2 varSigns arrSigns) signTimes
    | Div(a1, a2) -> cartesian (signEvalA a1 varSigns arrSigns) (signEvalA a2 varSigns arrSigns) signDiv
    | UMinus(a) -> Set.fold (fun state sign -> Set.add (signUnary sign) state) Set.empty (signEvalA a varSigns arrSigns)
    | Pow(a1, a2) -> cartesian (signEvalA a1 varSigns arrSigns) (signEvalA a2 varSigns arrSigns) signPow

let signAnd b1 b2 =
    Set.singleton(b1 && b2)

let signOr b1 b2 =
    Set.singleton(b1 || b2)

let signEQ sign1 sign2 =
    match sign1, sign2 with
    | (MINUSSIGN, MINUSSIGN) -> Set.empty.Add(true).Add(false)
    | (MINUSSIGN, _) -> Set.empty.Add(false)
    | (ZEROSIGN, ZEROSIGN) -> Set.empty.Add(true)
    | (ZEROSIGN, _) -> Set.empty.Add(false)
    | (PLUSSIGN, PLUSSIGN) -> Set.empty.Add(true).Add(false)
    | (PLUSSIGN, _) -> Set.empty.Add(false)

let signNEQ sign1 sign2 =
    Set.map not (signEQ sign1 sign2)

let signGT sign1 sign2 =
    match sign1, sign2 with
    | (MINUSSIGN, MINUSSIGN) -> Set.empty.Add(true).Add(false)
    | (MINUSSIGN, _) -> Set.empty.Add(false)
    | (ZEROSIGN, MINUSSIGN) -> Set.empty.Add(true)
    | (ZEROSIGN, ZEROSIGN) -> Set.empty.Add(false)
    | (ZEROSIGN, PLUSSIGN) -> Set.empty.Add(false)
    | (PLUSSIGN, PLUSSIGN) -> Set.empty.Add(true).Add(false)
    | (PLUSSIGN, _) -> Set.empty.Add(true)

let signGEQ sign1 sign2 =
    match sign1, sign2 with
    | (MINUSSIGN, MINUSSIGN) -> Set.empty.Add(true).Add(false)
    | (MINUSSIGN, _) -> Set.empty.Add(false)
    | (ZEROSIGN, PLUSSIGN) -> Set.empty.Add(false)
    | (ZEROSIGN, _) -> Set.empty.Add(true)
    | (PLUSSIGN, PLUSSIGN) -> Set.empty.Add(true).Add(false)
    | (PLUSSIGN, _) -> Set.empty.Add(true)
    
let signLT sign1 sign2 =
    match sign1, sign2 with
    | (MINUSSIGN, MINUSSIGN) -> Set.empty.Add(true).Add(false)
    | (MINUSSIGN, _) -> Set.empty.Add(true)
    | (ZEROSIGN, MINUSSIGN) -> Set.empty.Add(false)
    | (ZEROSIGN, ZEROSIGN) -> Set.empty.Add(false)
    | (ZEROSIGN, PLUSSIGN) -> Set.empty.Add(true)
    | (PLUSSIGN, PLUSSIGN) -> Set.empty.Add(true).Add(false)
    | (PLUSSIGN, _) -> Set.empty.Add(false)

let signLEQ sign1 sign2 =
    match sign1, sign2 with
    | (MINUSSIGN, MINUSSIGN) -> Set.empty.Add(true).Add(false)
    | (MINUSSIGN, _) -> Set.empty.Add(true)
    | (ZEROSIGN, MINUSSIGN) -> Set.empty.Add(false)
    | (ZEROSIGN, _) -> Set.empty.Add(true)
    | (PLUSSIGN, PLUSSIGN) -> Set.empty.Add(true).Add(false)
    | (PLUSSIGN, _) -> Set.empty.Add(false)

let rec signEvalB b varSigns arrSigns =
    match b with
    | T -> Set.singleton(true)
    | F -> Set.singleton(false)
    | BPar(b) -> signEvalB b varSigns arrSigns
    | And1(b1,b2) -> cartesian (signEvalB b1 varSigns arrSigns) (signEvalB b2 varSigns arrSigns) signAnd
    | Or1(b1,b2) -> cartesian (signEvalB b1 varSigns arrSigns) (signEvalB b2 varSigns arrSigns) signOr
    | And2(b1,b2) -> cartesian (signEvalB b1 varSigns arrSigns) (signEvalB b2 varSigns arrSigns) signAnd
    | Or2(b1,b2) -> cartesian (signEvalB b1 varSigns arrSigns) (signEvalB b2 varSigns arrSigns) signOr
    | NEG(b) -> Set.map not (signEvalB b varSigns arrSigns)
    | EQ(a1,a2) -> cartesian (signEvalA a1 varSigns arrSigns) (signEvalA a2 varSigns arrSigns) signEQ
    | NEQ(a1,a2) -> cartesian (signEvalA a1 varSigns arrSigns) (signEvalA a2 varSigns arrSigns) signNEQ
    | GT(a1,a2) -> cartesian (signEvalA a1 varSigns arrSigns) (signEvalA a2 varSigns arrSigns) signGT
    | GEQ(a1,a2) -> cartesian (signEvalA a1 varSigns arrSigns) (signEvalA a2 varSigns arrSigns) signGEQ
    | LT(a1,a2) -> cartesian (signEvalA a1 varSigns arrSigns) (signEvalA a2 varSigns arrSigns) signLT
    | LEQ(a1,a2) -> cartesian (signEvalA a1 varSigns arrSigns) (signEvalA a2 varSigns arrSigns) signLEQ

let signAnalysis action varSigns arrSigns =
    match action with
    | ActAssign(x, a) -> if (Map.containsKey x varSigns) then
                             let aSet = signEvalA a varSigns arrSigns
                             //(’a -> ’b -> ’a) -> ’a -> ’b set -> ’a
                             Set.fold (fun M expr -> Set.add ((Map.add x expr varSigns), arrSigns) M) Set.empty aSet
                         else
                             Set.empty
    | ActArrAssign(A, i, a) -> if ((Map.containsKey A arrSigns) && not (Set.isEmpty (Set.intersect (signEvalA i varSigns arrSigns) (set [ZEROSIGN; PLUSSIGN])))) then
                                   let aSet = signEvalA a varSigns arrSigns
                                   //(Set<Set<sign>> -> sign -> Set<Set<sign>>) -> Set<Set<sign>> -> sign set -> Set<Set<sign>>
                                   let setWithPrime = Map.find A arrSigns
                                   let setsWithoutPrime = Set.fold (fun state sign -> Set.add (Set.difference setWithPrime (Set.singleton(sign))) state) Set.empty setWithPrime
                                   let setsWithWithoutPrime = Set.add setWithPrime setsWithoutPrime
                                   let AReplacements = Set.fold (fun state aSign -> Set.union (Set.map (fun signSet -> Set.add aSign signSet) setsWithWithoutPrime) state) Set.empty aSet
                                   Set.fold (fun M signSet -> Set.add (varSigns, (Map.add A signSet arrSigns)) M) Set.empty AReplacements
                               else
                                   Set.empty
    | ActCheck(b) -> if (Set.contains true (signEvalB b varSigns arrSigns)) then
                         Set.singleton(varSigns, arrSigns)
                     else
                         Set.empty
    | ActSkip -> Set.singleton(varSigns, arrSigns)

let rec signAnalysisAlgorithm program mStart =
    let startNode = "qs"
    let endNode = "qe"
    let programGraph = edgesC(startNode, endNode, (parse program))
    let nodeList = Set.toList (Set.ofList (List.fold (fun state (sNode, _, eNode) -> sNode::eNode::state) [] programGraph))
    let mutable A = Map.add startNode mStart (Map.ofList (List.map (fun n -> (n, Set.empty)) nodeList))
    let mutable W = [startNode]
    while (not (List.isEmpty W)) do
        let q = List.head W
        W <- List.tail W
        for (qStart, action, qEnd) in (List.filter (fun (qs, _, _) -> qs = q) programGraph) do
            // TODO: For each varSign/arrSign tuple in A[qStart], run signanalysis for the action, and union the results together
            // The results go in A[qEnd], and qEnd should be added to A if A[qEnd] changes.
            let analAss = Set.fold (fun state (varSigns, arrSigns) -> Set.union state (signAnalysis action varSigns arrSigns)) Set.empty (Map.find qStart A)
            if not (Set.isSubset analAss (Map.find qEnd A)) then
                A <- Map.add qEnd (Set.union analAss (Map.find qEnd A)) A
                W <- W@[qEnd]
    A

let signToString sign =
    match sign with
    | PLUSSIGN -> "+"
    | ZEROSIGN -> "0"
    | _ -> "-"

let rec signListToString signList =
    match signList with
    | [sign] -> signToString sign
    | sign::rest -> (signToString sign) + ", " + (signListToString rest)
    | [] -> ""

let signSetToString signSet =
    signListToString (Set.toList signSet)

let signMemToString varSigns arrSigns =
    let vars = Map.fold (fun state key value -> state + key + ": " + (signToString value) + " ") "" varSigns
    Map.fold (fun state key value -> state + key + ": {" + (signSetToString value) + "} ") vars arrSigns

let printAnalAss (A:Map<string, Set<(Map<string, sign> * Map<string, Set<sign>>)>>) =
    for entry in A do
        printf "%s | " entry.Key
        for (varSigns, arrSigns) in entry.Value do
            printf "{%s} " (signMemToString varSigns arrSigns)
        printfn ""

//
// TASK 6 - noget med noget security
//
let rec fvA a =
    match a with
    | Num(n) -> Set.empty
    | Var(x) -> Set.singleton(x)
    | APar(a) -> fvA a
    | ArrayIndex(A, i) -> Set.union (Set.singleton(A)) (fvA i)
    | Plus(a1, a2) -> Set.union (fvA a1) (fvA a2)
    | Minus(a1, a2) -> Set.union (fvA a1) (fvA a2)
    | Times(a1, a2) -> Set.union (fvA a1) (fvA a2)
    | Div(a1, a2) -> Set.union (fvA a1) (fvA a2)
    | UMinus(a) -> fvA a
    | Pow(a1, a2) -> Set.union (fvA a1) (fvA a2)

let rec fvB b =
    match b with
    | T -> Set.empty
    | F -> Set.empty
    | BPar(b) -> fvB b
    | And1(b1,b2) -> Set.union (fvB b1) (fvB b2)
    | Or1(b1,b2) -> Set.union (fvB b1) (fvB b2)
    | And2(b1,b2) -> Set.union (fvB b1) (fvB b2)
    | Or2(b1,b2) -> Set.union (fvB b1) (fvB b2)
    | NEG(b) -> fvB b
    | EQ(a1,a2) -> Set.union (fvA a1) (fvA a2)
    | NEQ(a1,a2) -> Set.union (fvA a1) (fvA a2)
    | GT(a1,a2) -> Set.union (fvA a1) (fvA a2)
    | GEQ(a1,a2) -> Set.union (fvA a1) (fvA a2)
    | LT(a1,a2) -> Set.union (fvA a1) (fvA a2)
    | LEQ(a1,a2) -> Set.union (fvA a1) (fvA a2)

let rec secC (c:command) (X:Set<string>) (classification:Map<string,string>) (lattice:Map<string,Set<string>>) : bool =
    match c with
    | Assign(x, a) -> Set.forall (fun var -> Set.contains (Map.find x classification) (Map.find (Map.find var classification) lattice)) (Set.union X (fvA a))
    | ArrAssign(A,i,a) -> Set.forall (fun var -> Set.contains (Map.find A classification) (Map.find (Map.find var classification) lattice)) (Set.union (Set.union X (fvA i)) (fvA a))
    | Skip -> true
    | SemiColon(c1,c2) -> (secC c1 X classification lattice) && (secC c2 X classification lattice)
    | Iffi(gc) -> let (w, _) = secGC gc F X classification lattice
                  w
    | Dood(gc) -> let (w, _) = secGC gc F X classification lattice
                  w

and secGC gc d X classification lattice =
    match gc with
    | Pred(b,c) -> let w = secC c (Set.union (Set.union X (fvB b)) (fvB d)) classification lattice
                   (w, Or2(b, d))
    | Choice(gc1,gc2) -> let (w1, d1) = secGC gc1 d X classification lattice
                         let (w2, d2) = secGC gc2 d1 X classification lattice
                         (w1 && w2, d2)

let secAnal program classification lattice =
    let result = secC (parse program) (Set.empty) classification lattice
    printfn "%b" result

// Start interacting with the user
let program = "z:=0;
do y>0 -> z:=z+x;
y:=y-1
od"

let varMap = Map.ofList [("x", 3); ("y", 4); ("z", 0); ("n", 5); ("t", 0)]
let arrMap = Map.ofList [("A", [2])]

let startPhi = PAnd(PEQ(Var("x"), Num(3)), PEQ(Var("y"), Num(2)))
let endPhi = PEQ(Var("z"), Num(6))
let loopPhis = [PEQ(Var("z"), Times(Var("x"), Minus(Num(2), Var("y"))))]

let varSigns = Map.ofList [("x", MINUSSIGN); ("y", PLUSSIGN); ("z", PLUSSIGN); ("n", PLUSSIGN)]
let arrSigns = Map.ofList [("A", (Set.ofList [PLUSSIGN; ZEROSIGN; MINUSSIGN]))]
let mStart = Set.ofList [(varSigns, arrSigns)]

//verify program startPhi endPhi loopPhis

// Task 1: Pretty print program
//printAST program

// Task 2: Print program labels in Graphviz format
//fresh <- 1
//printLabels program

// Task 3: Print state for each step of the program
//fresh <- 1
//interpret program varMap arrMap

// Task 4: 
//fresh <- 1
//verify program startPhi endPhi loopPhis

// Task 5:
//fresh <- 1
//printAnalAss (signAnalysisAlgorithm program mStart)

// Task 6
let lattice = Map.ofList [("private", Set.singleton("private")); ("public", Set.ofList ["private"; "public"])]
let classification = Map.ofList [("x", "public"); ("y", "private"); ("z", "public")]
secAnal program classification lattice