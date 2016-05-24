module Helpers

open System
open Microsoft.FSharp.Reflection

open Models
open Algorithms

let toString (x:'a) = 
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name

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

let printMatrix (dim: int) (matrix: MapElement[,]) =
    [0..dim-1]
    |> List.map (fun i ->
                    [0..dim-1]
                    |> List.map (fun v -> matrix.[i,v])
                    |> List.fold (fun s el -> sprintf "%s\t%s" s (el |> toString)) "")
    |> List.fold (fun s el -> sprintf "%s\r\n%s" s el) ""

let storeMap (filePath: string) (dim: int) (matrix: MapElement[,]) = 
    let res = printMatrix dim matrix
    IO.File.WriteAllText(filePath, res)
