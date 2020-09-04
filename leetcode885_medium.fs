open System

//LeetCode,#885
//On a 2 dimensional grid with r rows and c columns, we start at (r0, c0) facing east.
//Here, the north-west corner of the grid is at the first row and column,
//and the south-east corner of the grid is at the last row and column.
//Now, we walk in a clockwise spiral shape to visit every position in this grid. 
//Whenever we would move outside the boundary of the grid, we continue our walk outside the grid
//Eventually, we reach all R * C spaces of the grid.
//Return a list of coordinates representing the positions of the grid in the order they were visited.

let spiralMat r c r0 c0 =
    //dir:direction to go,0:left,1:down,2:right,3:up
    //size is length of edge of spiral, and num1 is number of how many times to go the direction
    //num2 is number of visited grid
    //(x,y) is coordinate of now
    let rec spiral dir size num1 num2 (x,y)= 
        //all grid is visited
        if num2 > r*c then []
        //go same direction you go just before
        elif num1 < size then
            if dir = 0 then 
                if 0<=x && x<r && 0<=y && y<c then (x,y)::(spiral 0 size (num1+1)(num2+1)(x,y+1))
                else spiral dir size (num1+1) num2 (x,y+1)
            elif dir = 1 then
                if 0<=x && x<r && 0<=y && y<c then (x,y)::(spiral 1 size (num1+1)(num2+1)(x+1,y))
                else spiral dir size (num1+1) num2 (x+1,y)
            elif dir = 2 then
                if 0<=x && x<r && 0<=y && y<c then (x,y)::(spiral 2 size (num1+1)(num2+1)(x,y-1))
                else spiral dir size (num1+1) num2 (x,y-1)
            else
                if 0<=x && x<r && 0<=y && y<c then (x,y)::(spiral 3 size (num1+1)(num2+1)(x-1,y))
                else spiral dir size (num1+1) num2 (x-1,y)
        //change direction to go
        else
            if dir = 0 then spiral 1 size 0 num2 (x,y)
            elif dir = 1 then spiral 2 (size+1) 0 num2 (x,y)
            elif dir = 2 then spiral 3 size 0 num2 (x,y)
            else spiral 0 (size+1) 0 num2 (x,y)
    in
    spiral 0 1 0 1 (r0,c0)


[<EntryPoint>]
// test
let main argv =
    printfn "%A"(spiralMat 5 6 1 4)
    (*
    [(1, 4); (1, 5); (2, 5); (2, 4); (2, 3); (1, 3); (0, 3); (0, 4); (0, 5); (3, 5);
    (3, 4); (3, 3); (3, 2); (2, 2); (1, 2); (0, 2); (4, 5); (4, 4); (4, 3); (4, 2);
    (4, 1); (3, 1); (2, 1); (1, 1); (0, 1); (4, 0); (3, 0); (2, 0); (1, 0); (0, 0)]
    *)
    //This is OK(look at the page of this probrem, the answer is written).
    0  // return an integer exit code