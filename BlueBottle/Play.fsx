#I"../packages"
#r "FSharp.Data.2.3.0/lib/net40/FSharp.Data.dll"
#r @"bin/Debug/BlueBottle.exe"

open Algorithms
open Play
//Play.play ""

//open Queue
//let queue = Queue<int*int>()
//
//[1;2;7;5]
//|> List.iter (fun v -> queue.push (2,3) v)
//
//while queue.empty() |> not do
//    let (x,y);v=queue.pop()
//    printfn "(%i,%i)=%i" x y v

let pos = [|(0,0);(0,1);(0,2);(1,2);(1,1);(1,0);(2,0);(2,1);(2,2)|]
let mutable map = Map.empty.Add({x=0;y=0},None)
pos
|> Array.map (fun (x,y) -> {x=x;y=y})
|> Array.windowed 2
|> Array.iter (fun v -> map <- map.Add(v.[1],Some(v.[0])))

createRoute map {x=2;y=2}
