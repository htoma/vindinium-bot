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

type Game() = 
    static member ParseResponse (response: string) =
        let parsed = Parser.Parse response
        let myPos = { x=parsed.Hero.SpawnPos.X; y=parsed.Hero.SpawnPos.Y }
        Game.parseMap parsed.Game.Board.Size parsed.Game.Board.Tiles

    static member private ParseLine (dim: int) (line: char[]) =
        line
        |> Seq.splitInto dim
        |> Seq.map (fun v ->
                    match v.[0], v.[1] with
                    | '#','#' -> MapElement.Wood
                    | '@',_ -> MapElement.Hero
                    | '[',']' -> MapElement.Tavern
                    | '$','-' -> MapElement.GoldFree
                    | '$',_ -> MapElement.GoldTaken
                    | _ -> MapElement.Free)

    static member parseMap (dim: int) (content: string) =
        let els = content
                    |> Seq.splitInto dim
                    |> Seq.map (fun v -> Game.ParseLine dim v)
                    |> array2D
        {tiles=dim; els=els}


Game.ParseResponse (IO.File.ReadAllText ("doc.json"))
