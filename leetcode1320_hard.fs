open System
//LeetCode,#1320

//Given list of (x,y) coordinate.
//The distance between coordinates (x1,y1) and (x2,y2) is |x1 - x2| + |y1 - y2|.
//Return the minimum total distance to point such list on XY plane using only two fingers. 
//Note that the initial positions of your two fingers are considered free so don't count towards your total distance.

//In original probrem, grid is alphabet keyboard,and input is string.
//But conversion from alphabet to (x,y) coordinate is easy and complicated, so I omit it.

//definision of binary tree
type 'a tree = 
    |Lf 
    |Br of 'a * 'a tree *'a tree

let mindist list=
    let dist1 x list =
        match list with
        |[] -> 0
        |head::tail -> abs(fst(List.head list)- fst x)+abs(snd(List.head list)- snd x)
    in
    //make tree,left branch is "using finger 0",right branch is "using finger 1"
    let rec addtree tree list =
        match list with
        |[] -> tree
        |head::tail ->
            match tree with
            |Lf -> addtree(Br(([head],[],0),Lf,Lf))tail
            |Br((list0,list1,dist),_,_) -> 
                Br((list0,list1,dist),
                   addtree(Br((head::list0,list1,dist+(dist1 head list0)),Lf,Lf))tail,
                   addtree(Br((list0,(head::list1),dist+(dist1 head list1)),Lf,Lf))tail)
    in
    let third (_, _, c) = c
    let littleval a b = if (third a)< (third b) then a else b in
    //from leef to root, take smaller value and its path.
    let rec returnnode tree=
        match tree with
        |Br(a,Lf,Lf) -> a
        |Br(a,left,right) -> littleval (returnnode left) (returnnode right)
    in
    returnnode(addtree Lf (List.rev list))

[<EntryPoint>]
// test
let main argv =
    printfn "%A"(mindist [(1,1);(0,0);(2,3);(2,3);(4,0)])//6
    printfn "%A"(mindist [(4,0);(0,4);(0,0);(2,5)])//7
    0 // return an integer exit code
