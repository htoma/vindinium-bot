// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open FSharp.Data
open Microsoft.FSharp.Reflection
let toString (x:'a) = 
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name

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
    { dim: int
      tiles: MapElement[,] }

type State = 
    { finished: bool      
      pos: Pos
      map: Map
      gold: int
      maxGold: int
      test: string }

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
                    | '$', '-' -> MapElement.GoldFree
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
        printfn "Pos: %ix%i" myPos.x myPos.y
        let map = Game.ParseMap parsed.Game.Board.Size parsed.Game.Board.Tiles
        let finished = parsed.Game.Finished        
        let maxGold = parsed.Game.Heroes
                        |> Array.map (fun h -> h.Gold)
                        |> Array.max
        { finished=finished; pos=myPos; map=map; gold=parsed.Hero.Gold; maxGold=maxGold; test=parsed.Game.Board.Tiles }
        
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

let rec makeWebRequest (url: string) (query: (string*string) list) (state: State option) = 
    try
        let result = Http.Request(url=url, query=query, httpMethod="POST")
        match result.StatusCode, result.Body with
        | 200, HttpResponseBody.Text text -> text
        | rc, _ when rc >= 400 && rc < 500 -> failwithf "Http Response code: %i, Wrong key, game is already finished, too slow" rc
        | 500, _ -> failwith "Something wrong on the server side"
        | code, _ -> failwithf "Failed with HTTP code: %i" code
    with
        | :? Net.WebException as ex ->
            failwith "WebException %A" ex

let initiateGame (key: string) =
    let query = 
        [ "key", key
          "turns", string Game.Turns
          "map", "m1"]
    makeWebRequest Game.Url query

let makeMove (key: string) (move: Move) = 
    let query = 
        [ "key", key
          "dir", toString move]
    makeWebRequest Game.Url query

let play (key: string) =    
    let response = initiateGame key None
    let initial = Game.GetState response
    let rec playTurn (state: State) (turn: int) =
        if state.finished || turn=Game.Turns then
            printfn "Finished, your gold: %i and winner gold: %i" state.gold state.maxGold
        else
            let move = Game.ChooseMoveWalkie state
            let response = makeMove key move (Some(state))
            let state = Game.GetState response
            playTurn state (turn+1)
    playTurn initial 0

[<EntryPoint>]
let main argv = 
    play ""
    0 // return an integer exit code
