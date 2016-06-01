#I"../packages"
#r "FSharp.Data.2.3.0/lib/net40/FSharp.Data.dll"
#r @"bin/Debug/BlueBottle.exe"

open System
open FSharp.Data
open Algorithms
open Play
open System.Threading

let mutable wins = []
for i in 1..20 do
    try
        let (won,url) = Play.play ""
        wins<-(won,url)::wins
        Thread.Sleep 5000
    with
        | :? _ ->  ignore()