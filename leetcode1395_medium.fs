open System
//LeetCode,#1395
//Given int array r, this means soldiers rating (each soldiers have unique rarting)
//Make team of 3 soldiers (r[i] < r[j] < r[k]) or (r[i] > r[j] > r[k]) where (0 <= i < j < k < n).
//Return number of teams
let numteams r =
    let n = Array.length r - 1 in
    let makeijkth r  = [for i in 0 .. (n-2) -> [for j in (i+1) .. (n-1) -> [for k in (j+1) .. n -> [|i;j;k|]]]] in
    let rec f (a:int[] list) =
        match a with
        |[] -> 0
        |head::tail ->
        //add,if team satisfy (r[i] < r[j] < r[k]) or (r[i] > r[j] > r[k])
            if (r.[(head.[0])] < r.[(head.[1])] && r.[(head.[1])] < r.[(head.[2])]) 
            || (r.[(head.[0])] > r.[(head.[1])] && r.[(head.[1])] > r.[(head.[2])])
                then 1+(f tail)
                else f tail
    in
    let rec g b =
        match b with
        |[] -> 0
        |head::tail -> (f head)+(g tail)
    in
    let rec h c =
        match c with
        |[] -> 0
        |head::tail -> (g head)+(h tail)
    in
    h(makeijkth r)

[<EntryPoint>]
// test
let main argv =
    printfn "%A"(numteams [|2;5;3;4;1|])//3
    0 // return an integer exit code