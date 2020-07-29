open System
//LeetCode,#763
(*
A string str of lowercase English letters is given.
We want to partition this string into as many parts as possible so that each letter appears in at most one part,
and return a list of integers representing the size of these parts.
*)

let pl (str:String) =
    let charlist = Array.toList(str.ToCharArray(0,str.Length)) in
    //k=0,list1=list2=[],list=char list,return the first and last index of each alphabet
    //For example, ['a';'b';'c';'b'] -> [(0,0);(1,3);(2,2)] (the order of return list may be different)
    let rec makeindexlist k list list1 list2 =
        match list with
        |[] -> list2
        |head::tail ->
            if List.tryFind(fun x -> x=head)list1 = None
                then makeindexlist (k+1) tail (head::list1) ((k,k)::list2)
            else 
                let j = List.findIndex(fun x -> x=head)list1 in
                let newlist2 = (List.mapi(fun i x ->if i=j then (fst(list2.[i]),k) else x )list2) in
                makeindexlist (k+1) tail list1 newlist2
    in
    let check a b = if (snd a)<(fst b)||(snd b)<(fst a) then false else true in
    let con a b =(min (fst a) (fst b),max (snd a) (snd b)) in
    //add tuple to tuple list to change ranges
    let rec hojo list tpl =
        match list with
        |[] -> [tpl]
        |head::tail -> 
            if not (check head tpl) then head::(hojo tail tpl)
            else hojo tail (con head tpl)
    in
    let rec hojo1 list1 list2 =
        match list2 with
        |[] -> list1
        |head::tail -> hojo1 (hojo list1 head) tail
    in
    List.length(hojo1 [] (makeindexlist 0 charlist [] []))

[<EntryPoint>]
// test
let main argv =
    printfn "%A"(pl "ababcbacadefegdehijhklij")//ababcbaca,defegde,hijhklij, so 3
    0 // return an integer exit code