open System
//LeetCode,#1411
//We have n*3 grid
//and paint each cell of the grid with exactly one of the three colours: Red, Yellow or Green while making sure that no two adjacent cells have the same colour.
//Return the number of ways you can paint this grid.

let numOfWays n =
    //See the (n-1)th row,if it is (R,Y,R),we can add (Y,R,Y) or (G,R,G) or (Y,G,Y) or (G,R,Y) or (Y,R,G) under it,
    //if it is if it is (R,Y,G),we can add (Y,R,Y) or (Y,G,Y) or (G,R,Y) or (Y,G,R) under it.
    //a n is the number of ways of nth row is 2 colour, like (R,Y,R)
    //b n is the number of ways of nth row is 3 colour, like (R,Y,G)
    let rec a n = if n = 1 then 6 else 3*(a (n-1))+2*(b (n-1))
        and b n = if n = 1 then 6 else 2*(a (n-1))+2*(b (n-1))
    in
    (a n)+(b n)

[<EntryPoint>]
// test
let main argv =
    printfn "%A"(numOfWays 7)//106494
    0 // return an integer exit code
