open System
//LeetCode,#1409
//Given int list q and int m (>= max q).
//In the beginning, you have the permutation P=[1,2,3,...,m].
//For the current i, find the position of q[i] in the permutation P (indexing from 0) and then move this at the beginning of the permutation P. 
//Repeat this action begin i=0 end i=(length q - 1).
//Finaly,return the list of positions of q[i]
let processqueries q m =
    let rec hojo q p =
        match q with
        |[] -> []
        |head::tail ->
            let i = Array.findIndex(fun elem -> elem = head) p in
            //move p.[i] at the beginning of the permutation p
            let newp = [|for j in 0 .. (m-1) -> if j=0 then p.[i] elif j<=i then p.[j-1] else p.[j]|] in
            i::(hojo tail newp)
    in
    hojo q [|1 .. m|]

[<EntryPoint>]
// test
let main argv =
    printfn "%A"(processqueries [7;5;5;8;3] 8) //[6;5;0;7;5]
    0 // return an integer exit code
