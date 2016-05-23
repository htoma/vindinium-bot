﻿module Play

open System
open FSharp.Data
open Microsoft.FSharp.Reflection
open Algorithms

let toString (x:'a) = 
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name

type Parser = JsonProvider<"doc.json">

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
    { dim: int
      tiles: MapElement[,] }

type State = 
    { finished: bool      
      pos: Pos
      spawnPos: Pos
      map: Map
      gold: int
      maxGold: int
      playUrl: string
      showUrl: string
      turn: int }

let closestMine (pos: Pos) (map: Map) =
    map.tiles
    |> Array2D.mapi (fun i j v -> (i,j,v))
    |> Seq.cast<int*int*MapElement>
    |> Seq.filter (fun (i,j,v)-> (v,[MapElement.GoldFree;MapElement.GoldTaken]) ||> List.contains)
    |> Seq.sortBy (fun (i,j,v) -> {x=i;y=j} |> manhattanDistance pos)
    |> Seq.map (fun (i,j,_) -> {x=i;y=j})
    |> Seq.head

let pathElementValid (el: MapElement) =
    el=MapElement.Free || el=MapElement.Hero

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
                    | '$','-' -> MapElement.GoldFree
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
        { finished=finished
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

let makeWebRequest (url: string) (query: (string*string) list) = 
    try
        let result = Http.Request(url=url, query=query, httpMethod="POST")
        match result.StatusCode, result.Body with
        | 200, HttpResponseBody.Text text -> text
        | rc, _ when rc >= 400 && rc < 500 -> failwithf "Http Response code: %i, Wrong key, game is already finished, too slow" rc
        | 500, _ -> failwith "Something wrong on the server side"
        | code, _ -> failwithf "Failed with HTTP code: %i" code
    with
        | :? Net.WebException as ex ->
            failwithf "WebException %A" ex
            
let initiateGame (key: string) =
    let query = 
        [ "key", key
          "turns", string Game.Turns
          "map", "m1"]
    makeWebRequest Game.Url query

let makeMove (key: string) (url: string) (move: Move) = 
    let query = 
        [ "key", key
          "dir", toString move]
    makeWebRequest url query

let linkMove (pos: Pos) (target: Pos) =
    match pos.x-target.x,pos.y-target.y with
    | -1,_ -> Move.South
    | 1,_ -> Move.North
    | _,1 -> Move.West
    | _,-1 -> Move.East
    | _ -> Move.Stay

let play (key: string) =    
    // initiate game
    let response = initiateGame key

    // get initial state
    let initial = Game.GetState response

    // choose current target: the closest mine
    let currentTarget = closestMine initial.pos initial.map

    // choose path to follow
    let targetPath = 
        match (dijkstra initial.map.dim initial.map.tiles initial.pos currentTarget pathElementValid) with
        | None -> failwith "Could not find path"
        | Some path -> path

    let rec playTurn (state: State) (turn: int) (path: Pos list) =
        if state.finished || turn=Game.Turns then
            printfn "Finished, your gold: %i and winner gold: %i" state.gold state.maxGold
            printfn "Show at %s" state.showUrl
        else
            printfn "Turn: %i" state.turn
            let move,rest = 
                match path with
                | [] -> Move.Stay,[]
                | head::tail -> 
                    match state.pos with
                    | original when original=state.spawnPos -> 
                        (linkMove original path.Tail.Head),targetPath
                    | other -> (linkMove other path.Tail.Head),tail
                    
            let response = makeMove key state.playUrl move
            let state = Game.GetState response
            playTurn state (turn+1) rest
    playTurn initial 0 targetPath

