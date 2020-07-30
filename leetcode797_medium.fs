open System
//LeetCode,#797
(*
Given a directed, acyclic graph of N nodes. 
Find all possible paths from node 0 to node N-1, and return them in any order.

The graph is given as follows 
: the nodes are 0, 1, ..., graph.length - 1.  graph[i] is a list of all nodes j for which the edge (i, j) exists.
*)

//definision of listtree,this means tree which branch is list
type 'a listtree =
   |Llf
   |Lbr of 'a * 'a listtree list 

let apfst array =
    let n = Array.length array - 1 in
    //add node to tree until node has no paths
    let rec makeltree ltree (a:int list array) =
        match ltree with
        |Llf -> makeltree (Lbr([0],[])) a
        |Lbr(list,l) -> Lbr(list,List.map (fun x -> makeltree (Lbr(x::list,[])) a) (a.[(List.head list)]))
    in
    let rec findlist ltree =
        match ltree with
        |Llf -> []
        |Lbr(list,l) -> if List.head list = n then [list] else List.concat(List.map findlist l)
    List.map(fun a -> List.rev a)(findlist(makeltree Llf array))


[<EntryPoint>]
// test
let main argv =
    printfn "%A"(apfst [|[1;2]; [3]; [3]; []|] )//There are two paths: 0 -> 1 -> 3 and 0 -> 2 -> 3.
    0 // return an integer exit code
