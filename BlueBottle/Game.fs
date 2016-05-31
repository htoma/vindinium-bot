module Game

open System
open FSharp.Data
open Microsoft.FSharp.Reflection

open Models
open Algorithms
open Helpers

type Parser = JsonProvider<"doc.json">

type Game () =
    static member Random = System. Random()
    static member TrainingUrl = @"http://vindinium.org/api/training"
    static member ArenaUrl = @"http://vindinium.org/api/arena"
    static member Turns = 100
    static member private ParseLine (dim: int) (line: char[]) =
        line
        |> Seq.splitInto dim
        |> Seq.map ( fun v ->
                    match v.[0], v.[1] with
                    | '#', '#' -> MapElement.Wood
                    | '@',_ -> MapElement.Hero
                    | '[', ']' -> MapElement.Tavern
                    | '$','-' -> MapElement.GoldF
                    | '$',i -> MapElement.GoldHero (Int32.Parse(i.ToString()))
                    | _ -> MapElement.Free)

    static member private PosFromMove (pos: Pos) (move: Move) =
        match move with
        | Move.Stay -> pos
        | Move.North -> {pos with x=pos.x-1}
        | Move.East -> {pos with y=pos.y+1}
        | Move.South -> {pos with x=pos.x+1}
        | Move.West -> {pos with y=pos.y-1}

    static member private PosValid (pos: Pos) (map: Map) =
        pos.x >= 0 &&
        pos.x < map.dim &&
        pos.y >= 0 &&
        pos.y < map.dim &&
        map.tiles.[pos.x,pos.y]=MapElement.Free

    static member private ParseMap (dim: int) (content: string) =
        let els = content
                    |> Seq.splitInto dim
                    |> Seq.map ( fun v -> Game.ParseLine dim v)
                    |> array2D
        {dim=dim; tiles=els}

    static member GetState (response: string) =
        let parsed = Parser.Parse response
        let myPos = { x=parsed.Hero.Pos.X; y=parsed.Hero.Pos.Y }
        let map = Game.ParseMap parsed.Game.Board.Size parsed.Game.Board.Tiles
        let finished = parsed.Game.Finished        
        { finished=finished
          me={ id=parsed.Hero.Id
               elo=parsed.Hero.Elo
               pos={ x=parsed.Hero.Pos.X; y=parsed.Hero.Pos.Y }
               spawn={ x=parsed.Hero.SpawnPos.X; y=parsed.Hero.SpawnPos.Y } 
               life=parsed.Hero.Life
               mines=parsed.Hero.MineCount
               gold=parsed.Hero.Gold }
          heroes=parsed.Game.Heroes 
                 |> Array.map (fun v -> { id=v.Id
                                          elo=1
                                          pos={ x=v.Pos.X; y=v.Pos.Y }
                                          spawn={ x=v.SpawnPos.X; y=v.SpawnPos.Y } 
                                          life=v.Life
                                          mines=v.MineCount
                                          gold=v.Gold })
                 |> List.ofArray
          map=map
          playUrl=parsed.PlayUrl
          showUrl=parsed.ViewUrl
          turn=parsed.Game.Turn }

