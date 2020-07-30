open System
//LeetCode,#1382 

//definision of binary tree
type 'a tree = 
    |Lf 
    |Br of 'a * 'a tree *'a tree

//definision of binary tree with height of the shortest and the longest branchs
//use this to balance binary search tree
type 'a heighttree =
    |Hlf
    |Hbr of 'a * 'a heighttree *'a heighttree * int * int

//get height of shortest branch
let minh htree =
    match htree with
    |Hlf -> 0
    |Hbr(_,_,_,m1,_) -> m1

//get heght of longest branch
let maxh htree =
    match htree with
    |Hlf -> 0
    |Hbr(_,_,_,_,m2) -> m2

//add height and get binary tree with height
let rec addh tree =
    match tree with
    |Lf -> Hlf
    |Br(a,left,right) -> Hbr(a,addh left,addh right,min (minh (addh left))(minh (addh right)) + 1,max (maxh (addh left))(maxh (addh right)) + 1)

//from binary tree with height, delete height and get binary tree
let rec delh htlee =
    match htlee with
    |Hlf -> Lf
    |Hbr(a,left,right,_,_) -> Br(a,delh left,delh right)

//left rotate action, this keep binary "search" tree
let rotateL htree = 
    match htree with
    |Hbr(a,ht1,Hbr(b,ht2,ht3,_,_),_,_) -> Hbr(b,Hbr(a,ht1,ht2,1+min(minh ht1)(minh ht2),1+max(maxh ht1)(maxh ht2)),ht3,1+min(1+min(minh ht1)(minh ht2))(minh ht3),1+max(1+max(maxh ht1)(maxh ht2))(maxh ht3))
    |Hbr(a,left,right,min,max) -> Hbr(a,left,right,min,max)
    |Hlf -> Hlf

//right rotate action, this keep binary "search" tree
let rotateR htree =
    match htree with
    |Hbr(b,Hbr(a,ht1,ht2,_,_),ht3,_,_) -> Hbr(a,ht1,Hbr(b,ht2,ht3,1+min(minh ht2)(minh ht3),1+max(maxh ht2)(maxh ht3)),1+min(1+min(minh ht2)(minh ht3))(minh ht1),1+max(1+max(maxh ht2)(maxh ht3))(maxh ht1))
    |Hbr(b,left,right,min,max) -> Hbr(b,left,right,min,max)
    |Hlf -> Hlf

    

let balanceBST tree =
    //if htree is balanced then true, else false 
    let check htree =
        match htree with
        |Hlf -> true
        |Hbr(_,_,_,m1,m2) -> if m1=m2 || m1+1=m2 then true else false
    in
    let rec bal htree =
        match htree with
        |Hlf -> Hlf
        |Hbr(a,left,right,m1,m2) ->
            //If left branch is not balanced,do bal to left branch.
            if not(check left)
                then bal(Hbr(a,bal left,right,1+min(minh(bal left))(minh right),1+max(maxh(bal left))(maxh right)))
            //If right branch is not balanced,do bal to right branch.
            elif not(check right)
                then bal(Hbr(a,left,bal right,1+min(minh(bal right))(minh left),1+max(maxh(bal right))(maxh left)))
            //If left branch is shorter, then do left rotation.
            elif minh left < (maxh right)-1
                then bal(rotateL(Hbr(a,left,right,m1,m2)))
            //If right branch is shorter, then do right rotation.
            elif minh right < (maxh left)-1
                then bal(rotateR(Hbr(a,left,right,m1,m2)))
            //In this case, htree is balanced, so return htree
            else Hbr(a,left,right,m1,m2)
    in
    delh(bal(addh tree))

let testtree0 = Br(1,Lf,Br(2,Lf,Br(3,Lf,Br(4,Lf,Br(5,Lf,Br(6,Lf,Br(7,Lf,Br(8,Lf,Lf))))))))

[<EntryPoint>]
// test
let main argv =
    printfn "%A"(balanceBST testtree0)//Check this return is balanced.
    0  // return an integer exit code