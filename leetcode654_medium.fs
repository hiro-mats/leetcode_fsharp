open System
//LeetCode,#654
//Given list : integer list with no duplicates.
//Maximum tree is defined as follow
(*
The root is the maximum number in the array.
The left subtree is the maximum tree constructed from left part subarray divided by the maximum number.
The right subtree is the maximum tree constructed from right part subarray divided by the maximum number.
*)
//Make maximum tree and return it.

//definision of binary tree
type 'a tree = 
    |Lf 
    |Br of 'a * 'a tree *'a tree

let maxBT list =
    let rec addmaxBT tree a =
        match tree with
        |Lf -> Br(a,Lf,Lf)
        |Br(b,left,right) ->
            if a<b then Br(b,left,(addmaxBT right a))
            elif a>b then Br(a,Br(b,left,right),Lf)
            else Br(b,left,right)
    in
    //add head list to tree,recursively
    let rec maxBThojo tree list =
        match list with
        |[] -> tree
        |head :: tail -> maxBThojo (addmaxBT tree head) tail
    in
    maxBThojo Lf list

[<EntryPoint>]
// test
let main argv =
    printfn "%A"(maxBT [3;2;1;6;0;5])
    0 // return an integer exit code