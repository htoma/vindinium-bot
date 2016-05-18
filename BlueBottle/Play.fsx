#I "../packages"
#r "FSharp.Data.2.3.0/lib/net40/FSharp.Data.dll"

open System
open FSharp.Data

type Parser = JsonProvider<"doc.json">

type Pos =
    { x: int
      y: int }

type Hero = 
    { pos: Pos
      life: int
      spawn: Pos
      crashed: bool }

type Move =
    | Stay
    | North
    | East
    | South
    | West

type MapElement = 
    | Free
    | Wood
    | Tavern
    | GoldFree
    | GoldTaken
    | Hero

type Map = 
    { tiles: int
      els: MapElement[,] }
    with
    static member parseMap (dim: int) (content: string) =
        dim


let parseResponse (response: string) =
    let parsed = Parser.Parse response
    if parsed.Game.Finished || parsed.Hero.Crashed then ignore()
    else
        let myPos = { x=parsed.Hero.SpawnPos.X, y=parsed.Hero.SpawnPos.Y }        
 
let value = Parser.Parse(IO.File.ReadAllText ("doc.json"))
