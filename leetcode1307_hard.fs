open System

//LeetCode,#1307
//Given words:string list and result:string.
//You need to check if the equation is solvable under the following rules:
//1,Each character is decoded as one digit (0 - 9).
//2.Every pair of different characters they must map to different digits.
//3.Each words[i] and result are decoded as one number without leading zeros.
//4.Sum of numbers on left side (words) will equal to the number on right side (result). 
//If the equation is solvable, return the correspondence from alphabet to number, otherwise, return None.

// calculate m^n
let rec expo m n =
    if n = 0 then 1
    else m*(expo m (n-1))

//list0 - list1 as Set
let rec listminus list0 list1 =
    let rec hojo list0 a =
        match list0 with
        |[] -> []
        |head::tail -> if head = a then hojo tail a
                       else head::(hojo tail a)
    in
    match list1 with
    |[] -> list0
    |head::tail -> listminus (hojo list0 head) tail

let issolvable (words:string list) (result:string) =
    //By makelist, we get a list of alphabets used in result and words with no overlapping.
    let rec makelist0 word list1 k = 
        let rec hojo a list =
            match list with
            |[] -> [a]
            |head::tail -> if a = head then list else head::(hojo a tail)
        in
        let l = String.length word in
        if k=l then list1
        else makelist0 word (hojo (word.[k]) list1)(k+1)
    in
    let rec makelist words list1 =
        match words with
        |[] -> list1
        |head::tail -> makelist tail (makelist0 head list1 0)
    in
    let list1 = makelist(result::words)[] in
    let k = List.length list1 in
    //Make the function from the correspondence between alphabets and numbers.
    let makef (list1:char list) (list2:int list) =
        let f a =
            match(List.tryFindIndex((=)a))list1 with
            |None -> 0
            |Some j -> list2.[j]
        in
        f
    in
    //Transform string to number by function made by makef.
    //For example, f:[a;b;c]->[1;2;3], makenum f : "abc" -> 123. 
    let rec makenum f (str:string) =
        let l = String.length str in
        List.sum [for j in 0 .. (l-1) -> (expo 10 (l-j-1))*(f str.[j])]
    in
    let check list1 list2 = 
        let f = makef list1 list2 in
        List.sum(List.map (makenum f) words) = makenum f result
    let backpath list =
        match list with
        |[] -> []
        |(m,list2,list3,list4)::tail -> (m,list2,List.tail list3,listminus list4 [List.head list3])::tail
    in 
    //Search the correspondence by depth-first way.
    //list2 is list of numbers to correspond alphabets, and m is length of list2.
    //list3 is numbers that you can use next, and list 4 is numbers not used in list2.
    let rec getlist list0 =
        match list0 with
        |[] -> None
        |(m,list2,list3,list4)::tail ->
            if m=k && (check list1 list2) then Some(List.zip list1 list2)
            elif m=k || (m<k && list3=[]) then getlist (backpath tail)
            else
                let newlist2 = (List.head list3)::list2 in
                let newlist3 = listminus [0..9] newlist2 in
                getlist ((m+1,newlist2,newlist3,newlist3)::list0)
    in
    getlist [(0,[],[0..9],[0..9])]
 


[<EntryPoint>]
// test
let main argv =
    printfn "%A"(issolvable ["SEND";"MORE"] "MONEY" )
    //Output is Some[('M', 0); ('O', 9); ('N', 3); ('E', 4); ('Y', 6); ('S', 8); ('D', 2);('R', 1)]
    //"SEND" -> 8432, "MORE" -> 0916, and "MONEY" -> 09346 = 8432+0916, so OK.
    0  // return an integer exit code