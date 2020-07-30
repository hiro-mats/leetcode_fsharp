open System
//LeetCode,#1402
//Given int array,this means satisfaction level.
//You can reorder array and disorder rest in any order,
//and you get sum of [ newarray's level * index ]
//For example,Given [4;3;2;1], reorder this to [2;4;3], then you get 2*1+4*2+3*3=19
//Return the max sum and array to get max.

//definision of arraytree,this means tree which branch is array
type 'a arraytree =
   |Alf
   |Abr of 'a * 'a arraytree array

//remove ith element from array
let removeith i (array:'a[]) = Array.append array.[..(i-1)] array.[(i+1)..]

let maxsattice array =
    //make array tree, its node is value * path
    let rec maketree k array tree =
        let n = Array.length array - 1 in
        if array = [||] then tree
        else
            match tree with
            |Alf -> Abr((0,[||]),[|for i in 0 .. n -> maketree (k+1)(removeith i array)(Abr((array.[i],[|array.[i]|]),[||]))|])
            |Abr((value,path),_) -> 
                Abr((value,path),[|for i in 0 .. n -> maketree (k+1)(removeith i array)(Abr((value+k*(array.[i]),Array.append path [|array.[i]|]),[||]))|])
    in
    //get max value and its path recursively
    let rec getmax tree =
        match tree with
        |Alf -> (0,[||])
        |Abr(x,[||]) -> x
        |Abr(x,array) ->
            max x (Array.max(Array.map(fun a -> getmax a)array))
    in
    getmax(maketree 1 array Alf)


[<EntryPoint>]
// test
let main argv =
    printfn "%A"(maxsattice [|-1;-4;-5|])//(0,[||])
    printfn "%A"(maxsattice [|-2;5;-1;0;3;-3|])//(35,?)
    0 // return an integer exit code