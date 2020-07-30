open System
//LeetCode,#1284

(*
Given a binary matrix(matrix with all cells equal to 0 or 1 only) A. 
In one step, you can choose one cell and flip it and all the four neighbours of it if they exist (Flip is changing 1 to 0 and 0 to 1).
A pair of cells are called neighboors if they share one edge.

To make A to zero matrix by the step, return a binary matrix of if cell is 1 then do the step, if cell is 0 then not. 
If there are no choice to make zero matrix, return "Not found".
*)



let minflip a =
    let subflip x = if x=1 then 0 else 1 in
    let m = (Array.length a) - 1 in
    let n = (Array.length a.[0]) - 1 in
    let f (Some a) = a in
    let mattolist matrix =
        let rec mtlhojo (matrix:('a option)[,]) i j =
            if j=n+1 then []
            elif matrix.[i,j]= None then mtlhojo matrix i (j+1)
            else (f (matrix.[i,j]))::(mtlhojo matrix i (j+1))
        in
        List.concat[for i in 0 .. m -> mtlhojo matrix i 0]
    in
    //flip0,up,down,left,rightflip is for definite flip function,
    //it flips it and all the four neighbours of it if they exist.
    let flip0 k l (matrix:int [,]) = Array2D.init (m+1) (n+1) (fun i j -> if i=k && j=l then subflip (matrix.[k,l]) else matrix.[i,j]) in
    let upflip k l (matrix:int [,]) =
        if k=0 then matrix
        else Array2D.init (m+1) (n+1) (fun i j -> if i=k-1 && j=l then subflip (matrix.[k-1,l]) else matrix.[i,j])
    in
    let downflip k l (matrix:int [,]) =
        if k=m then matrix
        else Array2D.init (m+1) (n+1) (fun i j -> if i=k+1 && j=l then subflip (matrix.[k+1,l]) else matrix.[i,j])
    in
    let leftflip k l (matrix:int [,]) =
        if l=0 then matrix
        else Array2D.init (m+1) (n+1) (fun i j -> if i=k && j=l-1 then subflip (matrix.[k,l-1]) else matrix.[i,j])
    in
    let rightflip k l (matrix:int [,]) =
        if l=n then matrix
        else Array2D.init (m+1) (n+1) (fun i j -> if i=k && j=l+1 then subflip (matrix.[k,l+1]) else matrix.[i,j])
    in
    let flip k l ((matrix1:int [,]),(matrix2:int [,])) = 
        if matrix1.[k,l]=1 then None
        else Some (Array2D.init(m+1)(n+1)(fun i j -> if i=k && j=l then 1 else matrix1.[i,j]) ,rightflip k l (leftflip k l (downflip k l (upflip k l (flip0 k l matrix2)))))
    in
    let check0mat matlist = List.tryFind (fun a -> (snd a)=(Array2D.init (m+1) (n+1)(fun i j -> 0))) matlist in
    let rec minflip0 matlist = 
        if matlist = [] then printf "Not Found "
        elif check0mat matlist <> None then printfn "%A"(fst(f (check0mat matlist)))
        else do minflip0 (List.concat(List.map (fun a -> mattolist(Array2D.init (m+1)(n+1)(fun k l -> flip k l a))) matlist))
    in
    minflip0 [(Array2D.init (m+1) (n+1)(fun i j -> 0),Array2D.init(m+1)(n+1)(fun i j -> a.[i].[j]))]

let testmat0 = [|[|1;1;1|]
                 [|1;0;1|]
                 [|0;0;0|]|]

let testmat1 = [|[|1;0;0|]
                 [|1;0;0|]|]

[<EntryPoint>]
// test
let main argv =
    minflip testmat0//[|[|1;1;1|];[|1;0;1|];[|0;0;0|]|]
    minflip testmat1//Not found
    0 // return an integer exit code
