﻿module Play

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
        [ "key", key ]
          //"turns", string Game.Turns
          //"map", sprintf "m%i" (random.Next(1, 6))]
          //"map", "m5"]
    makeWebRequest Game.ArenaUrl query

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

let mineSelector (state: State) (el: MapElement)  = 
            match el with
            | MapElement.GoldHero i -> i<>state.me.id
            | MapElement.GoldF -> true
            | _ -> false

let tavernSelector (el: MapElement) = 
            el=MapElement.Tavern

let hasTavernNeighbors (pos: Pos) (state: State) =
    let nb = neighbors state.map.dim state.map.tiles pos
    nb
    |> List.exists (fun n -> tavernSelector state.map.tiles.[n.x,n.y])

let vulnerableHeroNearby (state: State) (neighbors: Pos list) =
    let heroes = state.heroes
                    |> List.filter (fun h -> neighbors |> List.contains h.pos)
                    |> List.sortBy (fun h -> h.life)
    match heroes with
    | [] -> None
    | _ ->
        let iHaveTavern = hasTavernNeighbors state.me.pos state
        if heroes |> List.exists (fun h -> 
                                        let hasTavern = hasTavernNeighbors h.pos state
                                        if (hasTavern && (iHaveTavern |> not)) || h.life>(state.me.life-20) 
                                        then false
                                        else true) 
        then None
        else
            Some heroes.Head.pos

let tavernNeighbors (state: State) (neighbors: Pos list) =
    if state.me.life<90 && state.me.gold>=2 then
        neighbors
        |> List.filter (fun n -> tavernSelector state.map.tiles.[n.x,n.y])
    else []

let mineToBeTakenNeighbors (state: State) (neighbors: Pos list) =
    neighbors
    |> List.filter (fun n -> mineSelector state state.map.tiles.[n.x,n.y])

let shouldSelect (el: MapElement) =
    [MapElement.Free;MapElement.Hero] |> List.contains el

let continueMoveToTarget (state: State) (action: Action) (path: Pos list) = 
    match path with
        | head::tail ->
            match head with
            | p when p=state.me.pos -> (linkMove state.me.pos tail.Head),action,tail
            | _ -> (linkMove state.me.pos head),action,path
        | [] -> failwith "No elements in the path to move to"

let newMoveToTarget (state: State) (action: Action) (targetPos: Pos) =
    let newPath = astar state.map.dim state.map.tiles state.me.pos targetPos shouldSelect
    match newPath with 
    | None | Some [] ->  
        printfn "No reachable element for %A" action
        Move.Stay,Action.NoAction,[]
    | Some [head] -> failwith "Element for %A uncaught in neighbours" action        
    | Some (head::tail) ->
        (linkMove state.me.pos tail.Head),action,tail
   
let moveToTavern (state: State) (action: Action) (path: Pos list) = 
    if action=Action.LookupDrink then
        printfn "Continuing to move to the tavern"
        continueMoveToTarget state action path
    else        
        let tavernPos = closestTarget state.me.pos state.map tavernSelector |> Option.get
        printfn "I chose to move to a tavern at: %ix%i" tavernPos.x tavernPos.y
        newMoveToTarget state Action.LookupDrink tavernPos

let rec moveToMine (state: State) (action: Action) (path: Pos list) = 
    if action=Action.LookupMine then
        let target=(path |> List.rev).Head
        if mineSelector state state.map.tiles.[target.x,target.y] then
            printfn "Continuing to move to the mine"
            continueMoveToTarget state action path
        else
            printfn "I already this mine %ix%i" target.x target.y
            moveToMine state Action.NoAction []
    else        
        let minePos = closestTarget state.me.pos state.map (mineSelector state)
        match minePos with
        | Some v -> 
            printfn "I chose to move to a mine that is not mine at: %ix%i" v.x v.y
            newMoveToTarget state Action.LookupMine v
        | None ->
            printfn "I got all mines, staying put"
            Move.Stay,Action.NoAction,[]

let getNextMove (state: State) (action: Action) (path: Pos list) = 
    let nb = neighbors state.map.dim state.map.tiles state.me.pos
    match (vulnerableHeroNearby state nb) with
    | Some h ->
        printfn "Found a vulnerable hero near me, I will attack him"
        (linkMove state.me.pos h),Action.Fight,[]
    | None ->
        match tavernNeighbors state nb with
        | head::tail ->
            printfn "Found a tavern near me, I'm thirsty and I have enough money to drink: %ix%i" head.x head.y
            (linkMove state.me.pos head),Action.Drink,[]
        | [] ->
            if state.me.life<=50 && state.me.gold>=2 then
                moveToTavern state action path
            else 
               match mineToBeTakenNeighbors state nb with
                | head::tail -> 
                    printfn "Found a mine which is not mine near me %ix%i" head.x head.y
                    (linkMove state.me.pos head),Action.Mine,[]
                | [] ->
                    moveToMine state action path
      
let play (key: string) = 
    // initiate game
    let response = initiateGame key

    // get initial state
    let initial = Game.GetState response

    let file = @"c:\temp\res.txt"
    storeMap file initial.map.dim initial.map.tiles

    printfn "Bot ID: %i" initial.me.id
    printfn "At: %ix%i" initial.me.pos.x initial.me.pos.y
    printfn "TV @ %s" initial.showUrl

    let rec playTurn (state: State) (turn: int) (action: Action) (path: Pos list) =
        printfn "[Turn: %i Health: %i Gold: %i/%i Elo: %i]" state.turn state.me.life state.me.gold (state.maxGold()) state.me.elo
        printfn "Current pos: %ix%i" state.me.pos.x state.me.pos.y
        printfn "Life: %i" state.me.life
        if state.finished then
            let won = state.me.gold = state.maxGold()
            printfn "Game Over, your gold: %i and winner gold: %i" state.me.gold (state.maxGold())
            won, initial.showUrl
        else
            let actualAction,actualPath = 
                if state.me.pos=state.me.spawn then Action.NoAction,[]
                else action,path
                    
            let move,newAction,newPath = getNextMove state actualAction actualPath

            printfn "Move: %A" move        
            let response = makeMove key state.playUrl move
            let state = Game.GetState response
            playTurn state (turn+1) newAction newPath
    playTurn initial 0 Action.NoAction []
