open System
//LeetCode,#1289
(*
Given a square grid of integers mat, 
a falling path with non-zero shifts is a choice of exactly one element from each row of arr,
such that no two elements chosen in adjacent rows are in the same column.

Return the minimum sum of a falling path with non-zero shifts and its path.
*)

//definision of arraytree,this means tree which branch is array
type 'a arraytree =
   |Alf
   |Abr of 'a * 'a arraytree array

//remove ith element from array
let removeith i (array:'a[]) = Array.append array.[..(i-1)] array.[(i+1)..]


let minsum mat=
    let mat2 = Array.map(fun row -> Array.mapi(fun j x ->(x,j))row)mat in
    //make arraytree,which dosen't have directly below element's branch of the grid
    let rec maketree tree mat=
        if mat=[||] then tree
        else 
            match tree with
            |Alf -> maketree(Abr((0,-1),Array.map(fun x ->Abr(x,[|Alf|]))(mat.[0])))(mat)
            |Abr(a,_) ->Abr(a,Array.map(fun x -> maketree (Abr(x,[|Alf|]))(mat.[1..])  )(removeith(snd a)(mat.[0])))
    in
    //get minimum sum from leaf to root
    let rec getmin tree=
        match tree with
        |Alf -> (0,[])
        |Abr(a,array) -> 
            let x = Array.min(Array.map(fun tree -> getmin tree)array) in
            ((fst x)+(fst a),a::(snd x))
    in
    getmin(maketree Alf mat2)

let testmat = [|[|1;2;3|]
                [|4;5;6|]
                [|7;8;9|]|]

[<EntryPoint>]
// test
let main argv =
    printfn "%A"(minsum testmat)//13
    0 // return an integer exit code