open System
//LeetCode,#753
//The password is a sequence of n digits where each digit can be one of the first n digits 0, 1, ..., n-1.
//While entering a password, the last k digits entered will automatically be matched against the correct password.
//For example, password is "345", if you type "12345", password is entered.
//Return any password of minimum length that is guaranteed at some point of entering it.

//from an array of arrays,  get an array whose elements is direct product of the arrays.
let rec arrayprod arrayarray =
    match arrayarray with
    |[||] -> [|[||]|]
    |[|_|] -> Array.map(fun x -> [|x|])(arrayarray.[0])
    |_ -> Array.concat(Array.map(fun b -> (Array.map(fun a -> Array.append [|a|] b)arrayarray.[0]))(arrayprod arrayarray.[1..]))

//remove ith element from array
let removeith i (array:'a[]) = Array.append array.[..(i-1)] array.[(i+1)..]

//remove a from array
let remfromarray a array =
    let rec hojo a (array:'a array) l i =
        if i>=l then array
        elif a<>array.[i] then hojo a array l (i+1)
        else hojo a (removeith i array) (l-1) i
    in
    hojo a array (Array.length array) 0

let cracksafe n k =
    let makenode n k = arrayprod[|for j in 0 ..(k-1) -> [|0..(n-1)|]|] in
    //Edge of directional graph, written by adjency list.
    let makeedge arrayarray =
        let edgeexist (array1:'a []) (array2:'a []) =
            let l=Array.length array2 - 1 in
            if array1.[1..]=array2.[..(l-1)] then true else false
        in
        let rec makeedge0 k a array =
            if k >= Array.length array then [||]
            elif a=array.[k] || not (edgeexist a array.[k]) then makeedge0 (k+1) a array
            else Array.append [|k|] (makeedge0 (k+1) a array)
        in
        Array.map(fun a -> makeedge0 0 a arrayarray)arrayarray
    in
    let node0 = makenode n k in
    let edge0 = makeedge(node0) in
    let updateedge edge x = Array.map(fun a -> remfromarray x a)edge in
    //m is count of moving, point is where we are,
    //edge is edge of directional graph, edges to points where we were are removed,
    //direction is the index of edge of now point,
    //For example, edge is [[1];[0];[0;1]] and you are in 2 and you go 1, direction is 1.
    let backpath path =
        match path with
        |[] -> []
        |(m,point,edge,direction)::tail -> (m,point,edge,1+direction)::tail
    in
    let rec getpath0 path =
        match path with
        |[] -> []
        |(_,point,_,_)::tail -> point::(getpath0 tail)
    //Make path of node list by depth-first way.
    let rec getpath (path:(int*int*int[][]*int)list) =
        match path with
        |[] -> []
        |(m,point,edge,direction)::tail ->
            if m = -1+int((float n)**(float k)) then getpath0 path
            elif Array.length(edge.[point]) <= direction then
                getpath(backpath(List.tail path))
            else
                let newpoint = edge.[point].[direction] in
                getpath((1+m,newpoint,updateedge edge newpoint,0)::path)
    in
    //Transform node list to password.
    let rec hojo array l =
        if l = Array.length array then ""
        else string(array.[l]) + hojo array (l+1)
    in
    let rec getpassword list =
        match list with
        |[] -> ""
        |[a] -> hojo(node0.[a])0
        |head::tail -> string(node0.[head].[0]) + getpassword tail
    in
    getpassword(List.rev(getpath [(0,0,updateedge edge0 0,0)]))


[<EntryPoint>]
// test
let main argv =
    printfn "%A"(cracksafe 2 3)
    //Output is "0001011100", this includes 000,001,010,011,100,101,110,111, so OK.
    0  // return an integer exit code