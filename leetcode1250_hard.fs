open System

//LeetCode,#1250
//The list is said to be good if you can obtain a sum of 1 from the list by any possible subset and multiplicand.
//Return True if the array is good otherwise return False.
//That is, if and only if the greatest common divisor of the list equal 1, list is good

// Euclidean algorithm
let rec ecld m n =
    if n%m = 0 then m
    else ecld (n%m) m

let rec isGoodArray list=
    match list with
    |[] -> false
    |[a] -> if a=1 then true else false
    |first::second::tail ->
        let g = ecld first second in
        if g=1 then true
        else isGoodArray(g::tail)

[<EntryPoint>]
// test
let main argv =
    printfn "%A"(isGoodArray [2;4;6])//false
    printfn "%A"(isGoodArray [10;12;15])//true
    0  // return an integer exit code