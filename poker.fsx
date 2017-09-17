
module List = 
    let rec insertions x = function
        | []             -> [[x]]
        | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

    let rec permutations = function
        | []      -> seq [ [] ]
        | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))


type Suit = Spade=0|Heart=1|Diamond=2|Club=3
type Rank = |Value of int
            |A
            |J 
            |Q
            |K
            |S  
            |SS 
            |Unkown
            static member GetAllRank() = [  for i in 2 .. 10 do yield Value i ;
                                            yield J 
                                            yield A
                                            yield K 
                                            yield Q
                                            yield S 
                                            yield SS]

type PokerCard= {
    Rank: Rank
    Suit: Suit
}   
let sortRules=  function | _ ,SS -> -1
                         | SS,_ -> 1
                         | _ ,S -> -1
                         | S,_ -> 1
                         | (Value 2), _ -> 1
                         | _,(Value 2) -> -1
                         | A , _ -> 1
                         | _ , A -> -1
                         | K , _ -> 1
                         | _ , K -> -1
                         | Q , _ -> 1
                         | _ , Q -> -1
                         | J , _ -> 1
                         | _ , J -> -1
                         | (Value x),(Value y) -> if x>y then 1 else -1
                         |Unkown,_ -> -1
                         |_,Unkown -> 1

let  string2Card rank suit=
    let StrSplit2list (s:string) = (s.Split ' ') |> Array.toList   
    let sortCards s = List.sortWith (fun x y -> sortRules (x.Rank,y.Rank)) s
    let str2Rank = function |"A" -> A |"J"-> J |"K"-> K |"Q"->Q|"s"->S|"S"->SS
                            |c when (int c) >=2 && (int c) <= 10->Value (int c)
                            | _-> Unkown
    let rec f  = function 
        | [] -> []
        | (x,y)::t->  {Rank= (str2Rank x) ;Suit=enum<Suit>(int(y))} :: (f t)
    f (List.zip (StrSplit2list rank) (StrSplit2list suit)) |> sortCards


let countSameCards s = 
    let rec f acc i= function
        |[] ->[]
        |[{Rank=x;Suit=_}] -> (x,i+1)::acc
        |{Rank=x;Suit=_}::({Rank=x';Suit=_}::_ as t) -> if x = x' then (f acc (i+1) t) 
                                                        else (f ((x,i+1)::acc) 0 t )
    f [] 0 s

let charInc = function |(Value x) when x > 2 && x < 10-> Value (x+1)
                       |(Value x) when x = 10 -> J
                       |J -> Q
                       |Q -> K
                       |K -> A
                       |_ -> Unkown

let rec simpleFlatten s = 
    match s with
        | [] -> []
        | x::t -> x @ (simpleFlatten t)

let pureAA x i=  
    List.choose (function
    |(rank,counter) when counter >= i -> Some (List.replicate i rank)
    |_ -> None) (countSameCards x) 



let excludeCards allCards p = 
    let sortP s = List.sortWith (fun x y -> sortRules (x,y)) s
    let rec f acc prest = function
        | [] -> acc
        | x::t -> if List.contains x.Rank prest then f acc (List.tail prest) t
                  else f (x::acc) prest t
    List.rev (f [] (sortP p) allCards)


let pickPure i s = 
    let r = pureAA  s i
    if List.isEmpty r then [] else r

let pickAiBj i j s= 
    let aaaa = pickPure i s
    let sortP s = List.sortWith (fun x y -> sortRules (x,y)) s
    [for l in aaaa do 
        let restCards = (excludeCards s l)
        let bb = (pickPure j restCards)
        for l2 in bb do  yield l @ l2] 
     |> List.map sortP
     |> Seq.distinct |> List.ofSeq

let pickABCDE x = 
    let rec f acc n= function
        [] -> []
        | [{Rank=x;Suit=_}] -> (List.rev (x::n))::acc
        | {Rank=x;Suit=_}::({Rank=y;Suit=_}::_ as t) ->  if charInc(x)= y then f acc (x::n) t
                                                         elif x = y then f acc n t
                                                         else f ((List.rev (x::n))::acc) [] t
    List.rev (f [] [] x) |> List.filter (fun x-> (List.length x)> 5 )    

let pickFuns = [pickAiBj 4 0; pickAiBj 4 1; pickAiBj 4 2
                pickAiBj 3 0; pickAiBj 3 1; pickAiBj 3 2
                pickAiBj 2 0; pickAiBj 1 0; pickABCDE]

let findAllPatterns s  = 
    [for f in pickFuns do
        yield f s]
    |>simpleFlatten 

let checkPatternLines s l =
    let lineExist s l= 
        let rec f acc lrest= function 
            |[] -> acc
            |x::t -> if List.contains x.Rank lrest then f (x.Rank::acc) (List.tail lrest) t 
                     else f acc lrest t
        let accValue = f [] l s
        (List.rev accValue) = l
    let rec f2 acc restS = function
        |[] -> acc 
        |x::t -> if (lineExist restS x) then f2 (x::acc) (excludeCards restS x) t
                 else f2 acc restS t
    List.rev (f2 [] s l)

let allPossiblePatternLines s = 
    let sortPatterns x = List.sortBy (fun elem -> -(List.length elem)) x
    (findAllPatterns s) 
        |> sortPatterns 
        |> checkPatternLines s

    //(findAllPatterns test) |>sortPatterns |> (checkPatternLines test)


//
//A AA AABBCC
//AAA AAABB AAABBBCD AAABBBCCDD AAAA AAAABB AAAABC AAAABBCC AAAABBBB ABCDEFGHI 
//AABBCC AAABBBCD AAABBBCCDD 
let testRank = "2 A K A Q J 10 9 9 7 6 6 7 7 8 5 6 4"
let testSuit = "1 2 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1"

let test = string2Card testRank testSuit

test |> List.iter (printfn "%A")
printfn "-----------------------------------------------------------------"
test |> findAllPatterns |> List.iter (printfn "%A")
printfn "-----------------------------------------------------------------"
test |> allPossiblePatternLines |> printfn "%A"
test |> allPossiblePatternLines |> Seq.length |> printfn "%A"

//find2 test  -> [(6,6);(7,7);(7,7);(9.9);(A,A)]
//excludeCards allcards [Value 3,Value 4,Value 5, Value  6]
