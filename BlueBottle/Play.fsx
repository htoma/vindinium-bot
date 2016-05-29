#I"../packages"
#r "FSharp.Data.2.3.0/lib/net40/FSharp.Data.dll"
#r @"bin/Debug/BlueBottle.exe"

open System
open FSharp.Data
open Algorithms
open Play

let mutable wins = []
for i in 1..10 do
    try
        let (won,url) = Play.play ""
        wins<-(won,url)::wins
    with
        | :? _ ->  ignore()