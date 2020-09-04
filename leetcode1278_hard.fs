open System

//LeetCode,#1278
//You are given a string str and an integer k.
//First, change some characters of str to other characters.
//Then divide str into k non-empty disjoint substrings such that each substring is palindrome.
//Return the minimal number of characters that you need to change to divide the string.

let palindromePartition k (str:string)=
    let ary = str.ToCharArray(0,str.Length) in
    //m is array length, and return the number of characters to that you need to change the array to palindrome 
    let rec palcheck m (ary:'a []) =
        if m=0 || m=1 then 0
        elif ary.[0] = ary.[(m-1)] then palcheck (m-2) ary.[1..(m-2)]
        else 1+(palcheck (m-2) ary.[1..(m-2)])
    in
    let rec kdevide k (ary:'a[]) =
        let l = Array.length ary in
        if k=1 then palcheck l ary
        else List.min[for j in 0 .. (l-k-1) -> palcheck(j+1)(ary.[0..j]) + (kdevide(k-1)(ary.[(j+1)..(l-1)]))]
    in
    kdevide k ary


[<EntryPoint>]
// test
let main argv =
    printfn "%A"(palindromePartition 2 "abaccde")//"aba","ccde" so 2
    printfn "%A"(palindromePartition 3 "abaccde")//"aba","cc","de" so 1
    0  // return an integer exit code