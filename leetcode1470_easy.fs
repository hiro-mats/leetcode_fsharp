open System
//LeetCode,#1470
//given the list [x1;x2;..;xn;y1;..yn] , shuffle this to [x1;y1;x2;y2;..;xn;yn]
let shuffle l =
    let n = (List.length l)/2 in
    let l1,l2= (l.[0..(n-1)]),(l.[n..(2*n)]) in
    let rec hojo l1 l2 =
        match l1 with
        |[] -> []
        |head::tail -> List.append[head;(List.head l2)](hojo tail (List.tail l2))
    in
    hojo l1 l2
    
[<EntryPoint>]
// test
let main argv =
    printfn "%A"(shuffle [2;5;1;3;4;7]) //[2;3;5;4;1;7]
    0 // return an integer exit code
