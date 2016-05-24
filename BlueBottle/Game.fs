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
    static member Url = @"http://vindinium.org/api/training"
    static member Turns = 100
    static member private ParseLine (dim: int) (line: char[]) =
        line
        |> Seq.splitInto dim
        |> Seq.map ( fun v ->
                    match v.[0], v.[1] with
                    | '#', '#' -> MapElement.Wood
                    | '@',_ -> MapElement.Hero
                    | '[', ']' -> MapElement.Tavern
                    | '$','-' -> MapElement.GoldMe
                    | '$',_ -> MapElement.GoldTaken
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
        let maxGold = parsed.Game.Heroes
                        |> Array.map (fun h -> h.Gold)
                        |> Array.max
        { id=parsed.Hero.Id
          finished=finished
          pos=myPos
          spawnPos={ x=parsed.Hero.SpawnPos.X; y=parsed.Hero.SpawnPos.Y }
          map=map
          gold=parsed.Hero.Gold
          maxGold=maxGold
          playUrl=parsed.PlayUrl
          showUrl=parsed.ViewUrl
          turn=parsed.Game.Turn }
        
    static member ChooseMoveWalkie (state: State) =
        let moves =
            [ Move.North
              Move.East
              Move.South
              Move.West ]
            |> List.map ( fun d -> d, (Game.PosFromMove state.pos d))
            |> List.filter ( fun (_,p) -> (Game.PosValid p state.map) )
            |> List.map ( fun (d,_) -> d)
        match moves.Length with
        | 0 -> Move.Stay
        | n -> moves.[ Game.Random.Next(0, n)]

