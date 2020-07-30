open System
//LeetCode,#980
(*
On a 2-dimensional grid, there are 4 types of squares:

1 represents the starting square.  There is exactly one starting square.
2 represents the ending square.  There is exactly one ending square.
0 represents empty squares we can walk over.
-1 represents obstacles that we cannot walk over.

Return the paths list of 4-directional walks from the starting square to the ending square,
that walk over every non-obstacle square exactly once.
*)

//definision of listtree,this means tree which branch is list
type 'a listtree =
   |Llf
   |Lbr of 'a * 'a listtree list 

let uniquepath maparray =
    let m = (Array.length maparray) - 1 in
    let n = (Array.length (maparray.[0])) - 1 in
    let point s= 
        let pointrow = Array.findIndex(fun x -> x=true) (Array.map(Array.exists(fun x -> x=s)) maparray) in
        (pointrow,Array.findIndex(fun x -> x=s)(maparray.[pointrow]))
    in
    //get coordinate of start and goal
    let start,goal = (point 1),(point 2) in
    let obscount = Array.sum(Array.map (fun x -> Array.length(Array.where(fun x -> x=(-1)) x))maparray) in
    let map2d = Array2D.init (m+1) (n+1) (fun i j -> maparray.[i].[j]) in
    //go- means, make point of now to (-1) and point to go to 1 
    let goup (k,l) (map:int[,]) = Array2D.init (m+1) (n+1) (fun i j -> if i=k && j=l then (-1) elif i=(k-1) && j=l then 1 else map.[i,j]) in
    let godown (k,l) (map:int[,]) = Array2D.init (m+1) (n+1) (fun i j -> if i=k && j=l then (-1) elif i=(k+1) && j=l then 1 else map.[i,j]) in
    let goleft (k,l) (map:int[,]) = Array2D.init (m+1) (n+1) (fun i j -> if i=k && j=l then (-1) elif i=k && j=(l-1) then 1 else map.[i,j]) in
    let goright (k,l) (map:int[,]) = Array2D.init (m+1) (n+1) (fun i j -> if i=k && j=l then (-1) elif i=k && j=(l+1) then 1 else map.[i,j]) in
    //Use hojoadd,get list of maps of go up, down, left, right(if you can do such move).
    let addup move (map:int[,]) count list =
        let (k,l)=List.head move in
        if k=0 || map.[k-1,l]<>0 then list
        else Lbr(((k-1,l)::move,goup (k,l) map,count+1),[])::list
    in
    let adddown move (map:int[,]) count list =
        let (k,l)=List.head move in
        if k=m || map.[k+1,l]<>0 then list
        else Lbr(((k+1,l)::move,godown (k,l) map,count+1),[])::list
    in
    let addleft move (map:int[,]) count list =
        let (k,l)=List.head move in
        if l=0 || map.[k,l-1]<>0 then list
        else Lbr(((k,l-1)::move,goleft (k,l) map,count+1),[])::list
    in
    let addright move (map:int[,]) count list =
        let (k,l)=List.head move in
        if l=n || map.[k,l+1]<>0 then list
        else Lbr(((k,l+1)::move,goright (k,l) map,count+1),[])::list
    in
    let hojoadd move map count list = 
        addright move map count (addleft move map count (adddown move map count (addup move map count list)))
    in
    //Make listtree of map.
    //If you cannot move, then branch is leef.
    let rec addtree ltree =
        match ltree with
        |Llf -> addtree (Lbr(([start],map2d,obscount),[]))
        |Lbr((move,map,count),l) -> Lbr((move,map,count),List.map addtree (hojoadd move map count []))
    in
    let near (a,b) (x,y) = if (a=x && (b=y-1||b=y+1))||(b=y && (a=x-1||a=x+1)) then true else false in
    //Search paths which satisfy probrem's condition, recursively.
    let rec findpath ltree =
        match ltree with
        |Llf -> []
        |Lbr((move,map,count),l) ->
            if (count+2 = (m+1)*(n+1)) && (near (List.head move) goal) then [goal::move]
            else List.concat(List.map findpath l)
    in
    List.map(fun a -> List.rev a)(findpath(addtree Llf))

let testmap0 = [|[|1;0;0;0|]
                 [|0;0;0;0|]
                 [|0;0;2;-1|]|]

let testmap1 = [|[|0;1|];[|2;0|]|]


[<EntryPoint>]
// test
let main argv =
    printfn "%A"(uniquepath testmap0)//1. (0,0),(0,1),(0,2),(0,3),(1,3),(1,2),(1,1),(1,0),(2,0),(2,1),(2,2)
                                    //2. (0,0),(1,0),(2,0),(2,1),(1,1),(0,1),(0,2),(0,3),(1,3),(1,2),(2,2)
    printfn "%A"(uniquepath testmap1)//[]
    0 // return an integer exit code