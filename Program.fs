// Solving probrems of LeetCode, and functions to solve them

open System

//test

// calculate factorial n
let rec fact n =
 if n=1||n=0 then 1 else n * (fact (n-1))


// calculate Permutation n k
let rec perm n k=
 if k=0 then 1 else n*(perm (n-1)(k-1))

// calculate Combination n k
let rec combi m n =
 if n=0 || n=m then 1
 else (combi (m-1) (n-1))+(combi (m-1) n)

// act f of k times to x 
let rec repeatf k f x =
 if k=0 then x
 else repeatf (k-1) f (f x)

// make matrix as directproduct of 2 arrays
let arrayprod array1 array2=Array.map(fun x ->(Array.map (fun y ->(x,y))array2))array1

// map from matrix to matrix like Array.map and List.map
let matmap f a = Array.map(fun b -> Array.map(fun x -> f x)b)a

// get max value of matrix like Array.max
let matmax a = 
 let n=(Array.length a)-1 in
 Array.max[|for i in 0 .. n -> Array.max a.[i]|]

//from 2 lists, get a list whose elements is direct product of the lists
let rec listprod2 list1 list2 =
 if List.length list1 = 1 then (List.map(fun y -> (List.head list1)::[y]) list2)
 else List.append (List.map(fun y -> (List.head list1)::[y]) list2) (listprod2 (List.tail list1) list2)


//from a list of lists,  get a list whose elements is direct product of the lists
let rec listprod listlist =
 match listlist with
 |[] -> [[]]
 |head::[] -> List.map(fun x -> [x])(head)
 |head::tail -> List.concat(List.map(fun b -> (List.map(fun a -> a::b)head))(listprod tail))

//[a0;a1;..;an]->[(f^n)a0;(f^(n-1))a1;..;an]
let rec listmap0 f list x =
 match list with
 |[] -> x
 |head::tail -> listmap0 f tail (f head x)

//remove ith element from array
let removeith i (array:'a[]) = Array.append array.[..(i-1)] array.[i..]

//definision of binaly tree
type 'a tree = 
    |Lf 
    |Br of 'a * 'a tree *'a tree

//definision of binaly tree with height of the shortest and the longest branchs
//use this to balance binary search tree
type 'a heighttree =
    |Hlf
    |Hbr of 'a * 'a heighttree *'a heighttree * int * int

//get height of shortest branch
let minh htree =
 match htree with
 |Hlf -> 0
 |Hbr(_,_,_,min,_) -> min

//get heght of longest branch
let maxh htree =
 match htree with
 |Hlf -> 0
 |Hbr(_,_,_,_,max) -> max

//add height and get binary tree with height
let rec addh tree =
 match tree with
 |Lf -> Hlf
 |Br(a,left,right) -> Hbr(a,addh left,addh right,min (minh (addh left))(minh (addh right)) + 1,max (maxh (addh left))(maxh (addh right)) + 1)

//from binary tree with height, delete height and get binary tree
let rec delh htlee =
 match htlee with
 |Hlf -> Lf
 |Hbr(a,left,right,_,_) -> Br(a,delh left,delh right)

//left rotate action, this keep binary "search" tree
let rotateL htree = 
 match htree with
 |Hbr(a,ht1,Hbr(b,ht2,ht3,_,_),_,_) -> Hbr(b,Hbr(a,ht1,ht2,min (minh ht1) (minh ht2) + 1,max (maxh ht1) (maxh ht2) + 1),ht3,min (min (minh ht1) (minh ht2) + 1) (minh ht3) + 1,max (max (maxh ht1) (maxh ht2) + 1) (maxh ht3) + 1)
 |Hbr(a,left,right,min,max) -> Hbr(a,left,right,min,max)
 |Hlf -> Hlf

//right rotate action, this keep binary "search" tree
let rotateR htree =
 match htree with
 |Hbr(b,Hbr(a,ht1,ht2,_,_),ht3,_,_) -> Hbr(a,ht1,Hbr(b,ht2,ht3,min (minh ht2) (minh ht3) + 1,max (maxh ht2) (maxh ht3) + 1),min (min (minh ht2) (minh ht3) + 1) (minh ht1) + 1,max (max (maxh ht2) (maxh ht3) + 1) (maxh ht1) + 1)
 |Hbr(b,left,right,min,max) -> Hbr(b,left,right,min,max)
 |Hlf -> Hlf

//definision of listtree,this means tree which branch is list
type 'a listtree =
   |Llf
   |Lbr of 'a * 'a listtree list 

//definision of arraytree,this means tree which branch is array
type 'a arraytree =
   |Alf
   |Abr of 'a * 'a arraytree array

//definision of matrixtree,this means tree which branch is matrix
type 'a mattree = 
    |Matlf
    |Matbr of 'a * 'a mattree [,] 


//get list from binary tree by inorder way
let rec inorder tree =
 match tree with
 |Lf -> []
 |Br(a,left,right) -> (inorder left)@(a::(inorder right))


let rec treetoarray1 tree =
 match tree with
 |Lf -> [|None|]
 |Br(a,left,right) -> Array.concat [[|Some a|];(treetoarray1 left);(treetoarray1 right)]

// map of Tree,like List.map
let rec treemap f tree =
 match tree with
 |Lf -> Lf
 |Br(a,left,right) -> Br(f a,treemap f left,treemap f right)

//add index to array
let arrayindexed ary = Array.mapi(fun i x -> (x,i)) ary

//add element to binary tree,this keep binary search tree
let rec addtree tree a =
 match tree with 
 |Lf -> Br(a,Lf,Lf)
 |Br(b,left,right) -> 
  if (snd a)<(snd b) then Br(b,addtree left a,right)
  elif (snd a)>(snd b) then Br(b,left,addtree right a)
  else Br(b,left,right)

//LeetCode,#1470
let shuffle a =
 let rec hojo a a0 a1 =
  match a with
  |[] -> (a0,a1)
  |head::tail -> (head::a0,(List.head tail)::a1)
 in
 hojo a [] []

let rec sntc a =
 let rec hojo a0 n i=
  match a0 with
  |[] -> i
  |head::tail -> if head < n then hojo tail n (i+1)
                             else hojo tail n i
 in
 match a with
 |[] -> []
 |head::tail -> hojo a head 0 ::sntc tail

let rec sum_i (tree: int tree) i =
 if i=0 then match tree with |Lf -> 0 |Br(a,_,_) -> a 
 else match tree with |Lf -> 0 |Br(_,left,right) -> (sum_i left (i-1))+(sum_i right (i-1))


let rec evg tree = 
 match tree with
 |Lf -> 0
 |Br(a,left,right) -> if a%2 =0 then (sum_i tree 2)+(evg left)+(evg right)
                      else (evg left)+(evg right)



let rec dls tree =
 match tree with
 |Lf -> 0
 |Br(a,Lf,Lf) -> a
 |Br(a,left,right) -> (dls left)+(dls right)


//LeetCode,#807
let miks (a: int array array) =
 let m = Array.length a - 1 in
 let n = (Array.length a.[0]) - 1 in
 let r0 = [|for j in 0 .. n -> Array.max[|for i in 0 .. m -> (a.[i]).[j] |]|] in
 let c0 = [|for i in 0 .. m -> Array.max[|for j in 0 .. n -> (a.[i]).[j] |]|] in
 let a0 i j = min(c0.[i])(r0.[j]) in
 let a1 i j = a0 i j - ((a.[i]).[j]) in
 Array.sum[|for i in 0 .. m -> Array.sum [|for j in 0 .. n -> a1 i j|]|]


//LeetCode,#1409
let processqueries q m =
 let n = Array.length q - 1 in
 let rec pqhojo k (a: int array) (p: int array) b = 
  if k>n then b
  else
  let i = Array.findIndex(fun elem -> elem=a.[k]) p in
  let newp = [|for j in 0 .. (m-1) -> if j=0 then p.[i] elif j<=i then p.[j-1] else p.[j]|] in
  let newb = i::b in
  pqhojo (k+1) a newp newb
 in
 pqhojo 0 q [|for j in 1 .. m -> j|] []

//LeetCode,#1395
let numteams r =
 let n = Array.length r - 1 in
 let makeijkth r  = [for i in 0 .. (n-2) -> [for j in (i+1) .. (n-1) -> [for k in (j+1) .. n -> [|i;j;k|]]]] in
 let rec f (a:int[] list) =
  match a with
  |[] -> 0
  |head::tail -> if (r.[(head.[0])]<r.[(head.[1])]&&r.[(head.[1])]<r.[(head.[2])])||(r.[(head.[0])]>r.[(head.[1])]&&r.[(head.[1])]>r.[(head.[2])])
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

//LeetCode,#654
let maxBT list =
 let rec addmaxBT tree a =
  match tree with
  |Lf -> Br(a,Lf,Lf)
  |Br(b,left,right) ->
   if a<b then Br(b,left,(addmaxBT right a))
   elif a>b then Br(a,Br(b,left,right),Lf)
   else Br(b,left,right)
 in
 let rec maxBThojo tree list =
  match list with
  |[] -> tree
  |head :: tail -> maxBThojo (addmaxBT tree head) tail
 in
 maxBThojo Lf list

//LeetCode,#1008
let tbst list1 list2 =
 let rec addtree tree a =
  match tree with
  |Lf -> Br((a,1),Lf,Lf)
  |Br(b,left,right) -> 
   if a<(fst b) then Br(b,addtree left a,right)
   elif a>(fst b) then Br(b,left,addtree right a)
   else Br((fst b,(snd b)+1),left,right)
 in
 let rec addtree1 tree list =
  match list with
  |[] -> tree
  |head::tail -> addtree1 (addtree tree head) tail
 in
 inorder(addtree1 (addtree1 Lf list1) list2)




let combi0 list =
 let rec hojo list n =
  match list with
  |[]|[_] -> 1
  |head::tail -> (combi n head)* (hojo tail (n-head))
 in
 hojo list (List.sum list)


let rec countlist list =
 let rec f list listlist =
  match list with
  |[] -> []
  |head::tail -> (List.map (fun x -> head::x) listlist)@(f tail listlist)
 in
 match list with
 |[] -> [[]]
 |head::tail -> f [0 .. head] (countlist tail)

let ltp list=
 List.sum(List.map(fun x -> combi0 x)(countlist list)) - 1

let pl lst =
 let check a b =
  if (snd a)<(fst b)||(snd b)<(fst a) then false else true in
 let con a b =(min (fst a) (fst b),max (snd a) (snd b)) in
 let rec hojo list tpl =
  match list with
  |[] -> [tpl]
  |head::tail -> 
   if not (check head tpl)then head::(hojo tail tpl)
   else hojo tail (con head tpl)
 in
 let rec hojo1 list1 list2 =
  match list2 with
  |[] -> list1
  |head::tail -> hojo1 (hojo list1 head) tail
 in
 hojo1 [] lst


let charnumlist (str:string) =
 let list0 = Array.toList(str.ToCharArray(0,str.Length)) in
 let rec addtree tree a =
  match tree with
  |Lf -> Br((a,1),Lf,Lf)
  |Br(b,left,right) -> 
   if a<(fst b) then Br(b,addtree left a,right)
   elif a>(fst b) then Br(b,left,addtree right a)
   else Br((fst b,(snd b)+1),left,right)
 in
 let rec maketree list tree=
  match list with
  |[] -> tree
  |head::tail -> maketree tail (addtree tree head)
 in
 inorder(maketree list0 Lf)


let minstep str1 str2=
 let rec countcom list a =
  match list with
  |[] -> 0
  |head::tail ->
   if (fst a)=(fst head) then min (snd a) (snd head)
   else countcom tail a
 in
 let rec countcom1 list1 list2=
  match list2 with
  |[] -> 0
  |head::tail -> (countcom list1 head)+countcom1 list1 tail
 in
 (String.length str1) - (countcom1 (charnumlist str1) (charnumlist str2))

//LeetCode,#1402
let maxsatice array =
 let n = Array.length array - 1 in
 let rec hojo ltree =
  match ltree with
  |Llf -> Lbr(([],array,0,0),List.map hojo [for j in 0 .. n -> Lbr(([array.[j]],removeith j array,array.[j],1),[])])
  |Lbr((list0,array0,sum,count),l) ->
   if count = n+2 then Lbr((list0,array0,sum,count),[])
   else Lbr((list0,array0,sum,count),List.map hojo [for j in 0 .. (n-count) -> Lbr((array0.[j]::list0,removeith j array0,sum + (count+1)*(array0.[j]),count+1),[])])        
 in
 let rec maketree ltree tree =
  match ltree with
  |Llf -> tree
  |Lbr((list0,array0,sum,count),l) -> listmap0 maketree l (addtree tree (list0,sum))
 in
 let rec findmax tree = 
  match tree with
  |Lf -> None
  |Br(a,left,Lf) -> Some a
  |Br(a,left,right) -> findmax right
 in
 findmax(maketree (hojo Llf) Lf)


//LeetCode,#980
let uniquepath maparray =
 let m = (Array.length maparray) - 1 in
 let n = (Array.length (maparray.[0])) - 1 in
 let point s= 
  let pointrow = Array.findIndex(fun x -> x=true) (Array.map(Array.exists(fun x -> x=s)) maparray) in
  (pointrow,Array.findIndex(fun x -> x=s)(maparray.[pointrow]))
 in
 let start,goal = (point 1),(point 2) in
 let obscount = Array.sum(Array.map (fun x -> Array.length(Array.where(fun x -> x=(-1)) x))maparray) in
 let map2d = Array2D.init (m+1) (n+1) (fun i j -> maparray.[i].[j]) in
 let goup (k,l) (map:int[,]) = Array2D.init (m+1) (n+1) (fun i j -> if i=k && j=l then (-1) elif i=(k-1) && j=l then 1 else map.[i,j]) in
 let godown (k,l) (map:int[,]) = Array2D.init (m+1) (n+1) (fun i j -> if i=k && j=l then (-1) elif i=(k+1) && j=l then 1 else map.[i,j]) in
 let goleft (k,l) (map:int[,]) = Array2D.init (m+1) (n+1) (fun i j -> if i=k && j=l then (-1) elif i=k && j=(l-1) then 1 else map.[i,j]) in
 let goright (k,l) (map:int[,]) = Array2D.init (m+1) (n+1) (fun i j -> if i=k && j=l then (-1) elif i=k && j=(l+1) then 1 else map.[i,j]) in
 let addup move (map:int[,]) count list =
  let (k,l)=List.head move in
  if k=0 || map.[k-1,l]<>0 then list
  else Lbr(((k-1,l)::move,goup (k,l) map,count+1),[])::list
 in
 let adddown move (map:int[,]) count list =
  let (k,l)=List.head move in
  if k=m || map.[k+1,l]<>0 then list
  else Lbr(((k+1,l)::move,godown (k,l) map,count+1),[])::list
 in
 let addleft move (map:int[,]) count list =
  let (k,l)=List.head move in
  if l=0 || map.[k,l-1]<>0 then list
  else Lbr(((k,l-1)::move,goleft (k,l) map,count+1),[])::list
 in
 let addright move (map:int[,]) count list =
  let (k,l)=List.head move in
  if l=n || map.[k,l+1]<>0 then list
  else Lbr(((k,l+1)::move,goright (k,l) map,count+1),[])::list
 in
 let hojoadd move map count list = addright move map count (addleft move map count (adddown move map count (addup move map count list))) in
 let rec addtree ltree =
  match ltree with
  |Llf -> addtree (Lbr(([start],map2d,obscount),[]))
  |Lbr((move,map,count),l) -> Lbr((move,map,count),List.map addtree (hojoadd move map count []))
 in
 let near (a,b) (x,y) = if (a=x && (b=y-1||b=y+1))||(b=y && (a=x-1||a=x+1)) then true else false in
 let rec findpath ltree =
  match ltree with
  |Llf -> []
  |Lbr((move,map,count),l) ->
   if (count+2 = (m+1)*(n+1)) && (near (List.head move) goal) then [goal::move]
   else List.concat(List.map findpath l)
 in
 findpath(addtree Llf)

let apfst array =
 let n = Array.length array - 1 in
 let rec makeltree ltree (a:int list array) =
  match ltree with
  |Llf -> makeltree (Lbr([0],[])) a
  |Lbr(list,l) -> Lbr(list,List.map (fun x -> makeltree (Lbr(x::list,[])) a) (a.[(List.head list)]))
 in
 let rec findlist ltree =
  match ltree with
  |Llf -> []
  |Lbr(list,l) -> if List.head list = n then [list] else List.concat(List.map findlist l)
 findlist(makeltree Llf array)

let rfp (str:string) =
 let rec hojo a list =
  match list with
  |[] -> ([],[])
  |head::tail -> if head = a then ([],tail) else (head::(fst(hojo a tail)),(snd(hojo a tail)))
 in
 let rec hyphencount k list =
  match list with
  |[] -> []
  |head::[] -> [(head,1)]
  |head1::head2::tail ->
   if head1 <> '-' then (head1,1)::(hyphencount 1 (head2::tail)) 
   elif head2 <> '-' then ('-',k)::(hyphencount 1 (head2::tail))
   else hyphencount (k+1) (head2::tail)
 in
 let rec subrfp k list =
  match list with
  |[] -> Lf
  |head::[] -> Br(head,Lf,Lf)
  |head::tail -> Br(head,subrfp(k+1)(fst(hojo ('-',k+1) (List.tail tail))),subrfp(k+1)(snd(hojo ('-',k+1) (List.tail tail))))
 in
 subrfp 0 (hyphencount 1 (Array.toList(str.ToCharArray(0,str.Length))))

//LeetCode,#1284
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
  elif check0mat matlist <> None then printfn "%A"(f (check0mat matlist))
  else do minflip0 (List.concat(List.map (fun a -> mattolist(Array2D.init (m+1)(n+1)(fun k l -> flip k l a))) matlist))
 in
 minflip0 [(Array2D.init (m+1) (n+1)(fun i j -> 0),Array2D.init(m+1)(n+1)(fun i j -> a.[i].[j]))]

//LeetCode,#1382 
let balanceBST tree =
 //if left branch is shorter than right branch,then do right rotate, if left is longer,do left rotate
 let rec bal htree =
  match htree with
  |Hlf -> Hlf
  |Hbr(a,left,right,m1,m2) ->
   if (minh (bal left))+1 < (maxh (bal right))
    then bal(rotateL (Hbr(a,bal left,bal right,min(minh(bal left))(minh(bal right)) + 1,max(maxh(bal left))(maxh(bal right)) + 1)))
   elif (minh (bal right))+1 < (maxh (bal left))
    then bal(rotateR (Hbr(a,bal left,bal right,min(minh(bal left))(minh(bal right)) + 1,max(maxh(bal left))(maxh(bal right)) + 1)))
   else Hbr(a,bal left,bal right,min(minh(bal left))(minh(bal right)) + 1,max(maxh(bal left))(maxh(bal right)) + 1)
 in
 bal(addh tree)

//LeetCode,
let matscore a =
 let m = (Array.length a) - 1 in
 let n = (Array.length a.[0]) - 1 in
 let rec hojo list0 list=
  match list with
  |[] ->[]
  |head::tail ->
   if list0 = [] then List.map(fun x -> [x])list
   elif List.head list0 >= head then hojo list0 tail
   else (head::list0)::(hojo list0 tail)
 in
 let hojo1 list listlist = List.concat(List.map(fun list0 -> hojo list0 list)listlist) in
 let rec hojo2 k list listlistlist=
  if k=List.length list then listlistlist
  else hojo2 (k+1) list ((hojo1 list (List.head listlistlist))::listlistlist)
 in
 let mat2 = arrayprod(List.toArray(List.concat ( hojo2 0 [0 .. (m-1)] [[[]]]))) (List.toArray(List.concat ( hojo2 0 [0 .. (n-1)] [[[]]]))) in
 let rec flip x = if x=1 then 0 elif x=0 then 1 else x in
 let rec fliprow mat list =
  match list with
  |[] -> mat
  |head::tail -> fliprow ([|for i in 0 .. m -> [|for j in 0 .. n -> if i = head then flip(mat.[i].[j]) else mat.[i].[j]|]|]) tail
 in
 let rec flipcolumn mat list =
  match list with
  |[] -> mat
  |head::tail -> flipcolumn ([|for i in 0 .. m -> [|for j in 0 .. n -> if j = head then flip(mat.[i].[j]) else mat.[i].[j]|]|]) tail
 in
 let flip mat (list1,list2) = flipcolumn (fliprow mat list1) list2
 let subscore (mat:int[][]) =
  let mat1 = [|for i in 0 .. m -> [|for j in 0 .. n -> int(2.0**(float(n-j)))*mat.[i].[j] |]|] in
  Array.sum[|for i in 0 .. m -> Array.sum(mat1.[i])|]
 in
 max(subscore a,([],[]))(matmax(matmap(fun x -> (subscore(flip a x),x))(arrayprod (List.toArray(List.concat(hojo2 0 [0..m] [[[]]]))) (List.toArray(List.concat(hojo2 0 [0..n][[[]]]))))))

//LeetCode,#1320
let mindist list=
 let dist1 x list =
  match list with
  |[] -> 0
  |head::tail -> abs(fst(List.head list)- fst x)+abs(snd(List.head list)- snd x)
 in
 //make tree,left branch is "using finger 0",right branch is "using finger 1"
 let rec addtree tree list =
  match list with
  |[] -> tree
  |head::tail ->
   match tree with
   |Lf -> addtree(Br(([head],[],0),Lf,Lf))tail
   |Br((list0,list1,dist),_,_) -> Br((list0,list1,dist),addtree(Br((head::list0,list1,dist+(dist1 head list0)),Lf,Lf))tail,addtree(Br((list0,(head::list1),dist+(dist1 head list1)),Lf,Lf))tail)
 in
 let third (_, _, c) = c
 let littleval a b = if (third a)< (third b) then a else b in
 let rec returnnode tree=
  match tree with
  |Br(a,Lf,Lf) -> a
  |Br(a,left,right) -> littleval (returnnode left) (returnnode right)
 in
 returnnode(addtree Lf list)

//LeetCode,#1467 
let getprob array=
 let n=(Array.sum array)/2 in
 let k=Array.length array in
 let rec hojo array=
  if Array.length array= 1 then array.[0]
  else array.[0]*(hojo array.[1..])
 in
 // check obtained array's type iqual rest array's type
 let check array1 =
  let array2 = Array.map2(fun x y -> x-y)array array1 in 
  if (Array.sort array1)=(Array.sort array2) then true else false
 in
 // how many pattern is possible to get array1
 let calcpattern array1 = 
  let hojo1 array=Array.map(fun x -> fact x)array in
  let array2 = Array.map2(fun x y -> x-y)array array1 in
  hojo(hojo1 array)/((hojo(hojo1 array1))*(hojo(hojo1 array2)))
 in
 //make array tree, which branch is "how many balls to get in one color?"
 let rec maketree j m tree=
  if j>=k then tree
  else
   match tree with
   |Alf -> maketree 0 n (Abr(Array.zeroCreate k,[|Alf|])) 
   |Abr(array1,_)->Abr(array1,[|for i in 0 .. (min m array.[j]) -> maketree (j+1) (m-i) (Abr([|for s in 0 .. (k-1) -> if s<j then array1.[s] elif s=j then i else 0|],[|Alf|])) |])
 in
 let rec sumpattern tree=
  match tree with
  |Alf -> 0
  |Abr(array1,[|Alf|]) ->
   if not(check array1) then 0
   else calcpattern array1
  |Abr(_,treearray) -> Array.sum(Array.map sumpattern treearray)
 in
 (float(sumpattern(maketree 0 n Alf)))/(float(combi (2*n) n))

//LeetCode,#1289
let minsum mat=
 let mat2 = Array.map(fun row -> Array.mapi(fun j x ->(x,j))row)mat in
 //make arraytree,which dosen't have directly below element's branch of matrix
 let rec maketree tree mat=
  if mat=[||] then tree
  else 
  match tree with
  |Alf -> maketree(Abr((0,-1),Array.map(fun x ->Abr(x,[|Alf|]))(mat.[0])))(mat)
  |Abr(a,_) ->Abr(a,Array.map(fun x -> maketree (Abr(x,[|Alf|]))(mat.[1..])  )(removeith(snd a)(mat.[0])))
 in
 let rec getmin tree=
  match tree with
  |Alf -> (0,[])
  |Abr(a,array) -> 
   let x = Array.min(Array.map(fun tree -> getmin tree)array) in
   ((fst x)+(fst a),a::(snd x))
 in
 getmin(maketree Alf mat2)






 


let tree0 = Br(6,Br(7,Br(2,Br(9,Lf,Lf),Lf),Br(7,Br(1,Lf,Lf),Br(4,Lf,Lf))),Br(8,Br(1,Lf,Lf),Br(3,Lf,Br(5,Lf,Lf))))
let bitree0 = Br(1,Lf,Br(2,Lf,Br(3,Lf,Br(4,Lf,Br(5,Lf,Br(6,Lf,Lf))))))
let skyline0 = [|[|3;0;8;4|];[|2;4;5;7|];[|9;2;6;3|];[|0;3;1;0|]|]

[<EntryPoint>]
let main argv =
   printfn "%A"(minsum[|[|1;2;3|];[|4;5;6|];[|7;8;9|]|]) 
   0  // return an integer exit code
