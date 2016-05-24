module Play

open System
open FSharp.Data
open Microsoft.FSharp.Reflection

open Models
open Algorithms
open Helpers
open Game
            
let initiateGame (key: string) =
    let random = Random(int DateTime.UtcNow.Ticks)
    let query = 
        [ "key", key
          "turns", string Game.Turns
          "map", sprintf "m%i" (random.Next(1, 6))]
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

    printfn "Bot ID: %i" initial.me.id
    printfn "At: %ix%i" initial.me.pos.x initial.me.pos.y
    printfn "TV @ %s" initial.showUrl

    // choose current target: the closest mine
    let mineSelector (el: MapElement) = 
        match el with
        | MapElement.GoldHero i -> i<>initial.me.id
        | MapElement.Free -> true
        | _ -> false
    let currentTarget = closestTarget initial.me.pos initial.map mineSelector

    printfn "Closest mine at: %ix%i" currentTarget.x currentTarget.y

    // choose path to follow
    let minePath = 
        match (dijkstra initial.map.dim initial.map.tiles initial.me.pos currentTarget pathElementValid) with
        | None -> failwith "Could not find path"
        | Some path -> path

    let showPath = minePath
                   |> List.map (fun pos -> sprintf "%ix%i" pos.x pos.y)
                   |> List.fold (fun s el -> s + "->" + el) ""

    printfn "Path: %s" showPath
    
    let rec playTurn (state: State) (turn: int) (path: Pos list) =
        printfn "Current pos: %ix%i" state.me.pos.x state.me.pos.y
        if state.finished || turn=Game.Turns then
            printfn "Finished, your gold: %i and winner gold: %i" state.me.gold (state.maxGold())
            printfn "Show at %s" state.showUrl
        else
            printfn "Turn: %i" state.turn
            let move,rest = 
                let currentPath = 
                    if state.me.pos=state.me.spawn then minePath
                    else path

                match currentPath with
                | [] -> Move.Stay,[]
                | [v] -> Move.Stay,[]
                | head::tail -> 
                    (linkMove state.me.pos tail.Head),tail
            printfn "Move: %A" move        
            let response = makeMove key state.playUrl move
            let state = Game.GetState response
            playTurn state (turn+1) rest
    playTurn initial 0 minePath

let vulnerableHeroNearby (state: State) (neighbors: Pos list) =
    let heroes = state.heroes
                    |> List.filter (fun h -> neighbors |> List.contains h.pos)
                    |> List.sortBy (fun h -> h.life)
    match heroes with
    | [] -> false
    | head::tail ->
        state.me.life>head.life && head.mines>0

let shouldTavern (state: State) (neighbors: Pos list) =
    state.me.life<80 &&
    state.me.gold>=2 &&
    neighbors
    |> List.exists (fun n -> state.map.tiles.[state.me.pos.x,state.me.pos.y]=MapElement.Tavern)

let moveToTavern (state: State) (neighbors: Pos list) = 
    let mineSelector (el: MapElement) = 
        match el with
        | MapElement.GoldHero i -> i<>state.me.id
        | MapElement.Free -> true
        | _ -> false
    let minePos = closestTarget state.me.pos state.map mineSelector
    printfn "I'm moving a mine which is not mine: %ix%i" minePos.x minePos.y
    let pos,_ = neighbors
                |> List.map (fun p -> p, manhattanDistance p minePos)
                |> List.minBy snd
    pos |> linkMove state.me.pos
      
let makeMove (state: State) = 
    let nb = neighbors state.map.dim state.map.tiles state.me.pos
    if vulnerableHeroNearby state nb then
        printfn "Found a vulnerable hero near me, I will attack him"
        Move.Stay
    else
        if shouldTavern state nb then
            printfn "Find a tavern near me, I'm thirsty and I have enough money to drink"
            Move.Stay
        else
            if state.me.life<=30 then
                moveToTavern state nb
            else find closest mine free or not mine
