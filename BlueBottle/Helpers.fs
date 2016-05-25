module Helpers

open System
open Microsoft.FSharp.Reflection
open FSharp.Data

open Models
open Algorithms

let toString (x:'a) = 
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name

let closestTarget (pos: Pos) (map: Map) (selector:MapElement->bool) =
    map.tiles
    |> Array2D.mapi (fun i j v -> (i,j,v))
    |> Seq.cast<int*int*MapElement>
    |> Seq.filter (fun (i,j,v)-> selector v)
    |> Seq.sortBy (fun (i,j,v) -> {x=i;y=j} |> manhattanDistance pos)
    |> Seq.map (fun (i,j,_) -> {x=i;y=j})
    |> Seq.tryPick Some

let pathElementValid (el: MapElement) =
    el=MapElement.Free || el=MapElement.Hero

let printMatrix (dim: int) (matrix: MapElement[,]) =
    [0..dim-1]
    |> List.map (fun i ->
                    [0..dim-1]
                    |> List.map (fun v -> matrix.[i,v])
                    |> List.fold (fun s el -> sprintf "%s\t%s" s (el |> toString)) (sprintf "%i\t" i))
    |> List.fold (fun s el -> sprintf "%s\r\n%s" s el) 
                    ([0..dim-1] |> List.fold (fun s i -> sprintf "%s\t\t%i" s i) "")

let storeMap (filePath: string) (dim: int) (matrix: MapElement[,]) = 
    let res = printMatrix dim matrix
    IO.File.WriteAllText(filePath, res)

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

