module Algorithms

open System
open Queue
open Models

type Map<'a, 'b when 'a : comparison> with
    member t.Update (key: 'a, value: 'b) =
        let tmp=t.Remove key
        t.Add(key,value)

let createRoute (cameFrom: Map<Pos,Pos option>) (target: Pos) =
    let rec search (route: Pos list) =
        match cameFrom.[route.Head] with
        | Some pos -> search (pos::route)
        | None -> route
    search [target]

let neighbors (dim: int) (matrix: 'a[,]) (pos: Pos) =
    [ {pos with x=pos.x-1}
      {pos with y=pos.y+1}
      {pos with x=pos.x+1}
      {pos with y=pos.y-1} ]
    |> List.filter (fun p -> p.x>=0 && p.x<dim && p.y>=0 && p.y<dim)

let manhattanDistance (start: Pos) (target: Pos) = 
    Math.Abs(start.x-target.x) + Math.Abs(start.y-target.y)

let dijkstra (dim: int) (matrix: 'a[,]) (start: Pos) (target: Pos) (shouldSelect: 'a->bool) =    
    let frontier = Queue<Pos>()
    frontier.push start 0
    let mutable cost = Map.empty.Add(start, 0)
    let mutable cameFrom = Map.empty.Add(start, None)
    let mutable visited = [] |> Set.ofList
    
    let rec search () =
        match frontier.empty() with
        | true -> None            
        | false ->
            let (current,value) = frontier.pop()
            visited<-visited.Add(current)
            if current=target then 
                let path = createRoute cameFrom target
                if path.Head=start then Some(path)
                else None
            else
                let nbs = neighbors dim matrix current
                nbs
                |> List.filter (fun p -> visited.Contains(p) |> not)
                |> List.filter (fun p -> shouldSelect matrix.[p.x,p.y]||p=target)
                |> List.iter (fun p ->
                            let newCost = manhattanDistance p target
                            if (cost.ContainsKey p |> not) ||
                                cost.[p]> newCost then
                                 cost<-cost.Update(p,newCost)
                                 cameFrom<-cameFrom.Update(p,Some(current))
                                 let priority = newCost
                                 frontier.push p newCost)                                 
                search()

    search()

  
let astar (dim: int) (matrix: 'a[,]) (start: Pos) (target: Pos) (shouldSelect: 'a->bool) =    
    let mutable openSet = [start] |> Set.ofList
    let mutable closedSet = [] |> Set.ofList
    let mutable cameFrom = Map.empty.Add(start, None)

    //the cost of getting from the start node to that node
    let mutable gScore = Map.empty.Add(start,0)

    //the cost of getting from start to target by passing through this node
    let fScore = Queue<Pos>()
    fScore.push start (manhattanDistance start target)
    
    let rec search () =
        match fScore.empty() with
        | true -> None
        | false ->
            let (current,value) = fScore.pop()
            if current=target then
                let path = createRoute cameFrom target
                if path.Head=start then Some(path)
                else None
            else
                openSet<-openSet.Remove(current)
                closedSet<-closedSet.Add(current)
                let nbs = neighbors dim matrix current
                nbs
                |> List.filter (fun p -> closedSet.Contains(p) |> not)
                |> List.filter (fun p -> shouldSelect matrix.[p.x,p.y]||p=target)
                |> List.iter (fun p -> 
                        let tmp = gScore.[current] + 1
                        let isNew = openSet.Contains p |> not 
                        if isNew then
                            openSet<-openSet.Add(p) // Discover a new node
                        if isNew || tmp < gScore.[p] then
                            // This path is the best until now. Record it!
                            cameFrom<-cameFrom.Update(p,Some(current))
                            gScore<-gScore.Update(p,tmp)
                            fScore.push p (gScore.[p] + (manhattanDistance p target)))
                search()
    search()
            



    
     
    


    