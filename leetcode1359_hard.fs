open System
//LeetCode,#1359
(*
Given n orders, each order consist in pickup and delivery services. 
Count all valid pickup/delivery possible sequences
such that delivery(i) is always after of pickup(i). 
*)

// calculate factorial n
let rec fact n =
    if n=1||n=0 then 1 else n * (fact (n-1))

let countOrders n=
    //Fix the order of pickup, [pickup(1),pickup(2),..,pickup(n)]
    //Deliverry(n) can set only after pickup(n),so one pattern.
    //Delivery(n-1) can set only after delivery(n-1), pickup(n-1), pickup(n), so three pattern.
    //So number of pattern of set delivery(n),..,delivery(1) is (2n-1)*(2n-3)*..*1.
    let rec hojo m =
        if m=1||m=0 then 1
        else m*(m-2)
    in
    (fact n)*(hojo(2*n-1))

[<EntryPoint>]
// test
let main argv =
    printfn "%A"(countOrders 3)//90
    0  // return an integer exit code
