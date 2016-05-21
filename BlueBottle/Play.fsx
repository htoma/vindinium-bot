#I"../packages"
#r "FSharp.Data.2.3.0/lib/net40/FSharp.Data.dll"
#r @"bin/Debug/BlueBottle.exe"

//open Play
//Play.play ""

open Queue
let queue = Queue<int*int>()

[1;2;7;5]
|> List.iter (fun v -> queue.push (2,3) v)

while queue.empty() |> not do
    let (x,y),v=queue.pop()
    printfn "(%i,%i)=%i" x y v