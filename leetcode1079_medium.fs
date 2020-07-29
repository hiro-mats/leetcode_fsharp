open System
//LeetCode,#1079
(*
You have a set of tiles, where each tile has one letter tiles[i] printed on it. 
Return the number of possible non-empty sequences of letters you can make.
*)

// calculate Combination m n
let rec combi m n =
    if n=0 || n=m then 1
    else (combi (m-1) (n-1))+(combi (m-1) n)

//Let N be sum of list, calculate (combi N list[0])*(combi (N-list[0]) list[1])*.. 
let combi0 list =
    let rec hojo list n =
        match list with
        |[]|[_] -> 1
        |head::tail -> (combi n head)* (hojo tail (n-head))
    in
    hojo list (List.sum list)


let ltp (str:String) = 
    let charlist = Array.toList(str.ToCharArray(0,str.Length)) in
    //make list of number of each letter
    let rec makenumlist list list1 list2 =
        match list with
        |[] -> list2
        |head::tail ->
            if List.tryFindIndex(fun a -> a = head) list1 = None 
                then makenumlist tail (head::list1) (1::list2)
            else
                let j = List.findIndex(fun a -> a = head) list1 in
                makenumlist tail list1 (List.mapi(fun i a -> if i = j then (a+1) else a)list2)
    in
    //From int list,make all sublists list
    //For example,[1;2] -> [[0;0];[0;1];[0;2];[1;0];[1;1];[1;2]]
    let rec countlist list =
        let rec f list listlist =
            match list with
            |[] -> []
            |head::tail -> (List.map (fun x -> head::x) listlist)@(f tail listlist)
        in
        match list with
        |[] -> [[]]
        |head::tail -> f [0 .. head] (countlist tail)
    in
    List.sum(List.map(fun x -> combi0 x)(countlist(makenumlist charlist [][]))) - 1

[<EntryPoint>]
// test
let main argv =
    printfn "%A"(ltp "abbccc")//188
    0 // return an integer exit code