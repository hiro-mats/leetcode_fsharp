open System
//LeetCode,#1467 
(*
Given 2n balls of k distinct colors. 
You will be given an integer array balls of size k where balls[i] is the number of balls of color i. 
All the balls will be shuffled uniformly at random, 
then we will distribute the first n balls to the first box and the remaining n balls to the other box.

We want to calculate the probability that the two boxes have the same number of distinct balls.
*)

// calculate factorial n
let rec fact n =
    if n=1||n=0 then 1 else n * (fact (n-1))

// calculate Combination n k
let rec combi m n =
    if n=0 || n=m then 1
    else (combi (m-1) (n-1))+(combi (m-1) n)

//definision of arraytree,this means tree which branch is array
type 'a arraytree =
   |Alf
   |Abr of 'a * 'a arraytree array

let getprob array=
    let n=(Array.sum array)/2 in
    let k=Array.length array in
    let rec hojo array=
        if Array.length array= 1 then array.[0]
        else array.[0]*(hojo array.[1..])
    in
    // check obtained array's type iqual rest array's type
    let check array1 =
        let array2 = Array.map2(fun x y -> x-y)array array1 in 
        if (Array.sort array1)=(Array.sort array2) then true else false
    in
    // how many pattern is possible to get array1
    let calcpattern array1 = 
        let hojo1 array=Array.map(fun x -> fact x)array in
        let array2 = Array.map2(fun x y -> x-y)array array1 in
        hojo(hojo1 array)/((hojo(hojo1 array1))*(hojo(hojo1 array2)))
    in
    //make array tree, which branch is "how many balls to get in one color?"
    let rec maketree j m tree=
        if j>=k then tree
        else
            match tree with
            |Alf -> maketree 0 n (Abr(Array.zeroCreate k,[|Alf|])) 
            |Abr(array1,_) -> 
                Abr(array1,[|for i in 0 .. (min m array.[j]) -> maketree (j+1) (m-i) (Abr([|for s in 0 .. (k-1) -> if s<j then array1.[s] elif s=j then i else 0|],[|Alf|])) |])
    in
    //calculate number of patterns from leaf to root
    let rec sumpattern tree=
        match tree with
        |Alf -> 0
        |Abr(array1,[|Alf|]) ->
            if not(check array1) then 0
            else calcpattern array1
        |Abr(_,treearray) -> Array.sum(Array.map sumpattern treearray)
    in
    (float(sumpattern(maketree 0 n Alf)))/(float(combi (2*n) n))

[<EntryPoint>]
// test
let main argv =
    printfn "%A"(getprob [|1;2;1;2|])//0.60000
    printfn "%A"(getprob [|3;2;1|])//0.30000
    0 // return an integer exit code