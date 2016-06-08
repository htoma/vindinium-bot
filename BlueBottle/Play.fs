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

let isGameWonInAdvance (state: State) = 
    let mines = countElementsOfType state.map (mineSelector state)
    state.me.mines >= mines &&
    state.me.gold > (state.heroes
                     |> List.map (fun h -> h.gold)
                     |> List.max)

let needToRefresh (state: State) =
    state.me.life <= Game.MaxLife-2*Game.HitPower

let tavernSelector (el: MapElement) = 
            el=MapElement.Tavern

let hasTavernNeighbors (pos: Pos) (state: State) =
    let nb = neighbors state.map.dim state.map.tiles pos
    nb
    |> List.exists (fun n -> tavernSelector state.map.tiles.[n.x,n.y])

let vulnerableHeroNearby (pos: Pos) (state: State) =
    let nb = neighbors state.map.dim state.map.tiles pos
    state.heroes
    |> List.exists (fun h -> nb |> List.contains h.pos && h.mines > 0)

let computeMyHittingPower (pos: Pos) (state: State) =
    if hasTavernNeighbors pos state 
    then state.me.life + Game.TavernRefresh
    else state.me.life

let computeOtherTotalHittingPower (pos: Pos) (state: State) =
    //compute total neighbour power
    let nb = neighbors state.map.dim state.map.tiles pos
    let hnb = state.heroes
              |> List.filter (fun h -> nb |> List.contains h.pos && h.life > Game.HitPower)
    printfn "I have %i heroes that I won't kill near me" (hnb |> List.length)
    let hWithTavern = hnb
                      |> List.filter (fun h -> hasTavernNeighbors h.pos state)
    printfn "I have %i heroes near me that have taverns near them" (hWithTavern |> List.length)
    let hnbPower = (hnb |> List.length) * Game.HitPower + (hWithTavern |> List.length) * Game.TavernRefresh

    //compute 2 steps away hero power
    let nbTwoStep = neighbors2StepAway state.map.dim state.map.tiles pos
    let hnbTwoStep = state.heroes
                     |> List.filter (fun h -> nbTwoStep |> List.contains h.pos)
    printfn "I have %i heroes at two steps away and they are ready to attack me" (hnbTwoStep |> List.length)
    let hWithTavernTwoStep = hnbTwoStep
                            |> List.filter (fun h -> hasTavernNeighbors h.pos state)
    printfn "I have %i heroes at two steps that have taverns near them" (hWithTavernTwoStep |> List.length)
    let hnbPowerTwoStep =  (hnbTwoStep |> List.length) * Game.HitPower + (hWithTavernTwoStep |> List.length) * Game.TavernRefresh

    hnbPower + hnbPowerTwoStep

let isSafePos (pos: Pos) (state: State) =
    (computeMyHittingPower pos state) >= (computeOtherTotalHittingPower pos state)

let isValidFightingPos (pos: Pos) (state: State) =
    (vulnerableHeroNearby pos state) &&
        (isSafePos pos state)

let shouldAttack (state: State) =
    let nb = neighbors state.map.dim state.map.tiles state.me.pos
    nb
    |> List.tryPick (fun p -> if isValidFightingPos p state then Some p else None)

let tavernNeighbors (state: State) (neighbors: Pos list) =
    if state.me.gold>=2 then
        neighbors
        |> List.filter (fun n -> tavernSelector state.map.tiles.[n.x,n.y])
    else []

let mineToBeTakenNeighbors (state: State) (neighbors: Pos list) =
    neighbors
    |> List.filter (fun n -> mineSelector state state.map.tiles.[n.x,n.y])

let shouldSelect (el: MapElement) =
    [MapElement.Free;MapElement.Hero] |> List.contains el

let getPossibleStarts (pos: Pos) (state: State) = 
    neighbors state.map.dim state.map.tiles pos
    |> List.filter (fun p -> state.map.tiles.[p.x,p.y]=MapElement.Free)
    |> List.filter (fun pn -> isSafePos pos state)

let newMoveToTarget (state: State) (action: Action) (elType: MapElement) (selector:MapElement->bool) =
    let possibleStarts = getPossibleStarts state.me.pos state
    match possibleStarts with
    | [] -> 
        printfn "No possible starts for pos %ix%i" state.me.pos.x state.me.pos.y
        Move.Stay,Action.NoAction,[]
    | _ -> 
        let targetPos = elementsOfType state.map selector
        let paths = targetPos
                    |> Seq.map (fun p -> p, astar state.map.dim state.map.tiles state.me.pos p shouldSelect)
                    |> Seq.filter (fun (p, path) -> path |> Option.isSome)
                    |> Seq.map (fun (p, path) -> p,path|> Option.get)
                    |> Seq.sortBy (fun (p,path) -> path.Length)
                    |> Seq.map (fun (p,_) -> p)
                    |> List.ofSeq
        match paths with
        | [] -> 
            printfn "Could not found any target of type %A" elType
            Move.Stay,Action.NoAction,[]
        | _ ->
            let closestTargetPos = paths.Head
            printfn "I chose to move to a %A at: %ix%i" elType closestTargetPos.x closestTargetPos.y            
            let bestStart = possibleStarts
                            |> List.minBy (fun p -> manhattanDistance p closestTargetPos)
            let newPath = astar state.map.dim state.map.tiles bestStart closestTargetPos shouldSelect
            match newPath with 
            | None | Some [] ->  
                printfn "No reachable element for %A" elType
                Move.Stay,Action.NoAction,[]
            | Some path ->
                (linkMove state.me.pos bestStart),action,path

let continueMoveToTarget (state: State) (action: Action) (path: Pos list) (elType: MapElement) (selector:MapElement->bool) = 
    match path with
        | head::tail ->
            if (isSafePos head state |> not) then
                printfn "Need to recompute path because continuing is not safe"
                newMoveToTarget state action elType selector
            else
                match head with
                | p when p=state.me.pos -> (linkMove state.me.pos tail.Head),action,tail
                | _ -> (linkMove state.me.pos head),action,path
        | [] -> failwith "No elements in the path to move to"
    
let moveToTavern (state: State) (action: Action) (path: Pos list) = 
    if action=Action.LookupDrink then
        printfn "Continuing to move to the tavern"
        continueMoveToTarget state action path MapElement.Tavern tavernSelector
    else
        printfn "Move to tavern interrupted due to another action: %A" action
        newMoveToTarget state Action.LookupDrink MapElement.Tavern tavernSelector

let rec moveToMine (state: State) (action: Action) (path: Pos list) = 
    printfn "Move to mine"
    if action=Action.LookupMine then
        let target=(path |> List.rev).Head
        if mineSelector state state.map.tiles.[target.x,target.y] then
            printfn "Continuing to move to the mine"
            continueMoveToTarget state action path MapElement.GoldF (mineSelector state)
        else
            printfn "Move to mine interrupted, no good mine found"
            newMoveToTarget state Action.LookupMine MapElement.GoldF (mineSelector state)
    else
        printfn "I have to search a new mine, previous action was: %A" action
        newMoveToTarget state Action.LookupMine MapElement.GoldF (mineSelector state)

let mine (state: State) (action: Action) (path: Pos list) = 
    let nb = neighbors state.map.dim state.map.tiles state.me.pos    
    match mineToBeTakenNeighbors state nb with
    | head::tail -> 
        printfn "Found a mine which is not mine near me %ix%i" head.x head.y
        (linkMove state.me.pos head),Action.Mine,[]
    | [] ->
        moveToMine state action path

let getNextMove (state: State) (action: Action) (path: Pos list) = 
    if isValidFightingPos state.me.pos state then
        printfn "I'll stay as I may win a battle near me"
        Move.Stay,Action.Fight,[]
    else
        match shouldAttack state with
        | Some pos ->
            (linkMove state.me.pos pos), Action.Fight,[]
        | None -> 
            let nb = neighbors state.map.dim state.map.tiles state.me.pos
            let isWonInAdvance = isGameWonInAdvance state
            if hasTavernNeighbors state.me.pos state then
                if needToRefresh state then
                    match tavernNeighbors state nb with
                    | head::tail ->
                        printfn "Found a tavern near me, I'm thirsty and I have enough money to drink: %ix%i" head.x head.y
                        (linkMove state.me.pos head),Action.Drink,[]
                    | [] -> failwith "No tavern near me"
                else 
                    if isWonInAdvance then
                        Move.Stay,Action.NoAction,[]
                    else
                        mine state action path
            else
                if isWonInAdvance || (needToRefresh state) then
                    moveToTavern state action path
                else 
                    mine state action path
      
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
        printfn "[Turn: %i Health: %i Gold: %i/%i Mines: %i Elo: %i]" state.turn state.me.life state.me.gold (state.maxGold()) state.me.mines state.me.elo
        printfn "Current pos: %ix%i" state.me.pos.x state.me.pos.y
        printfn "Life: %i" state.me.life
        if state.finished then
            let won = state.me.gold >= state.maxGold()
            printfn "Game Over, your gold: %i and others' gold: %i" state.me.gold (state.maxGold())
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
