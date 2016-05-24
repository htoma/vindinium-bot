module Play

open System
open FSharp.Data
open Microsoft.FSharp.Reflection

open Models
open Algorithms
open Helpers
open Game

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
          "map", "m2"]
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

    storeMap @"c:\temp\res.txt" initial.map.dim initial.map.tiles

    printfn "Bot ID: %i" initial.id
    printfn "At: %ix%i" initial.pos.x initial.pos.y
    printfn "TV @ %s" initial.showUrl

    // choose current target: the closest mine
    let currentTarget = closestMine initial.pos initial.map

    printfn "Closest mine at: %ix%i" currentTarget.x currentTarget.y

    // choose path to follow
    let minePath = 
        match (dijkstra initial.map.dim initial.map.tiles initial.pos currentTarget pathElementValid) with
        | None -> failwith "Could not find path"
        | Some path -> path

    let showPath = minePath
                   |> List.map (fun pos -> sprintf "%ix%i" pos.x pos.y)
                   |> List.fold (fun s el -> s + "->" + el) ""

    printfn "Path: %s" showPath
    
    let rec playTurn (state: State) (turn: int) (path: Pos list) =
        printfn "Current pos: %ix%i" state.pos.x state.pos.y
        if state.finished || turn=Game.Turns then
            printfn "Finished, your gold: %i and winner gold: %i" state.gold state.maxGold
            printfn "Show at %s" state.showUrl
        else
            printfn "Turn: %i" state.turn
            let move,rest = 
                let currentPath = 
                    if state.pos=state.spawnPos then minePath
                    else path

                match currentPath with
                | [] -> Move.Stay,[]
                | [v] -> Move.Stay,[]
                | head::tail -> 
                    (linkMove state.pos path.Tail.Head),tail
                    
            let response = makeMove key state.playUrl move
            let state = Game.GetState response
            playTurn state (turn+1) rest
    playTurn initial 0 minePath

