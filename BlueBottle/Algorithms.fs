﻿module Algorithms

open Queue

type Map<'a, 'b when 'a : comparison> with
    member t.Update (key: 'a, value: 'b) =
        let tmp=t.Remove key
        t.Add(key,value)

type Pos =
    { x: int
      y: int }

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
                match path.Head with
                | start -> Some(path)
                | _ -> None
            else
                neighbors dim matrix current
                |> List.filter (fun p -> visited.Contains(p) |> not)
                |> List.filter (fun p -> shouldSelect matrix.[p.x,p.y])
                |> List.iter (fun p ->
                            let newCost = cost.[current]+1
                            if (cost.ContainsKey current |> not) ||
                                cost.[current]> newCost then
                                 cost<-cost.Update(current,newCost)
                                 cameFrom<-cameFrom.Update(p,Some(current))
                                 frontier.push p newCost)                                 
                search()

    search()

    
    


    