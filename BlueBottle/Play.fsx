#I"../packages"
#r "FSharp.Data.2.3.0/lib/net40/FSharp.Data.dll"
#r @"bin/Debug/BlueBottle.exe"

open System
open FSharp.Data
open Algorithms
open Play

//let dim = 6
//let matrix = 
//        [ MapElement.Free //(0,0)
//          MapElement.Free //(0,1)
//          MapElement.Free //(0,2)
//          MapElement.Free //(0,3)
//          MapElement.Free //(0,4)
//          MapElement.Wood //(0,5)
//          MapElement.Free //(1,0)
//          MapElement.Wood //(1,1)
//          MapElement.Wood //(1,2)
//          MapElement.Free //(1,3)
//          MapElement.Wood //(1,4)
//          MapElement.Free //(1,5)
//          MapElement.Free //(2,0)
//          MapElement.Free //(2,1)
//          MapElement.Free //(2,2)
//          MapElement.Free //(2,3)
//          MapElement.Free //(2,4)
//          MapElement.Free //(2,5)
//          MapElement.Free //(3,0)
//          MapElement.Free //(3,1)
//          MapElement.Free //(3,2)
//          MapElement.Free //(3,3)
//          MapElement.Free //(3,4)
//          MapElement.Wood //(3,5)
//          MapElement.Free //(4,0)
//          MapElement.Free //(4,1)
//          MapElement.Free //(4,2)
//          MapElement.Wood //(4,3)
//          MapElement.Wood //(4,4)
//          MapElement.Free //(4,5)
//          MapElement.Free //(5,0)
//          MapElement.Free //(5,1)
//          MapElement.Free //(5,2)
//          MapElement.Free //(5,3)
//          MapElement.Free //(5,4)
//          MapElement.Free //(5,5) 
//          ]
//          |> List.splitInto dim
//          |> array2D
//
    
//dijkstra 6 matrix {x=0;y=0} {x=5;y=5} check

//let check (el: MapElement) =
//    el=MapElement.Free || el=MapElement.Hero
//
//type Parser = JsonProvider<"doc.json">
//let doc = IO.File.ReadAllText @"C:\work\vindinium-bot\BlueBottle\doc.json"
//let state = Game.GetState doc
//let target = closestMine state.pos state.map
//dijkstra state.map.dim state.map.tiles state.pos target check

Play.play ""
