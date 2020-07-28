open System
//LeetCode,#807
//given 2-dimensional grid ,its elements is height of building.
//make new grid ,which is the same as the original grid when viewed from all four directions of the grid,
//and its total sum is maximum.
//return the sum of difference between orginal grid and new grid.

let miks (a: int array array) =
    let m = Array.length a - 1 in
    let n = (Array.length a.[0]) - 1 in
    let r0 = [|for j in 0 .. n -> Array.max[|for i in 0 .. m -> (a.[i]).[j] |]|] in
    let c0 = [|for i in 0 .. m -> Array.max[|for j in 0 .. n -> (a.[i]).[j] |]|] in
    let a0 i j = min(c0.[i])(r0.[j]) in
    let a1 i j = a0 i j - ((a.[i]).[j]) in
    Array.sum[|for i in 0 .. m -> Array.sum [|for j in 0 .. n -> a1 i j|]|]

let testgrid = [|[|3;0;8;4|]
                 [|2;4;5;7|]
                 [|9;2;6;3|]
                 [|0;3;1;0|]|]
    
[<EntryPoint>]
// test
let main argv =
    printfn "%A"(miks testgrid) //35
    0 // return an integer exit code