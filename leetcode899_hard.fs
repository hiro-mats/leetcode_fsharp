open System
//LeetCode,#899
//A string str of lowercase letters is given.  Then, we may make any number of moves.
//In each move, we choose one of the first k letters (starting from the left), remove it, and place it at the end of the string.
//Return the lexicographically smallest string we could have after any number of moves.

//make string from char array
let makestr (array:char[])=
    let rec hojo k l (array:char[]) =
        if k=l then ""
        else array.[k].ToString()+(hojo (k+1) l array)
    in
    hojo 0 (Array.length array) array

// act f of k times to x 
let rec repeatf k f x =
    if k=0 then x
    else repeatf (k-1) f (f x)

let orderlyQueue (str:string) k =
    let ary = str.ToCharArray(0,str.Length) in
    //If k is not 1,we have f:[a0,a1,..,an]->[a1,a2,..,an,a0] and g:[a0,a1,..,an]->[a0,a2,a3,..,an,a1] .
    //So we have h=(g^(n-2)f):[a0,a1,..,an]->[a1,a0,a2,..,an].
    //So we can make all permutation of array by f and h (by Symmetric group theory).
    if k>1 then makestr(Array.sort ary)
    else
        let n = Array.length ary - 1 in
        let cyc (ary:'a []) = [|for j in  0 .. n -> if j=n then ary.[0] else ary.[j+1]|] in
        makestr(List.min[for j in 0 ..n -> repeatf j cyc ary])


[<EntryPoint>]
// test
let main argv =
    printfn "%A"(orderlyQueue "abcaad" 1)//"aadabc"
    printfn "%A"(orderlyQueue "abcaad" 2)//"aaabcd"
    0  // return an integer exit code