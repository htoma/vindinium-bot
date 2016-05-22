open System

let transposeCombine m =
    (m, m) ||> Map.fold (fun acc k1 m' ->
        (acc, m') ||> Map.fold (fun acc' k2 v ->
        acc'
        |> Map.add k2 (Map.add k1 v (defaultArg (acc' |> Map.tryFind k2) Map.empty))
    ))

type City =
    | Boise   | LosAngeles | NewYork | Seattle
    | StLouis | Phoenix    | Boston  | Chicago
    | Denver

let distanceBetweenCities =
  Map.ofList
    [
        (Boise, Map.ofList [(Seattle, 496);(Denver, 830);(Chicago, 1702)]);
        (Seattle, Map.ofList [(LosAngeles, 1141);(Denver, 1321)]);
        (LosAngeles, Map.ofList [(Denver, 1022);(Phoenix, 371)]);
        (Phoenix, Map.ofList [(Denver, 809);(StLouis, 1504)]);
        (Denver, Map.ofList [(StLouis, 588);(Chicago, 1009)]);
        (Chicago, Map.ofList [(NewYork, 811);(Boston, 986)]);
        (StLouis, Map.ofList [(Chicago, 300)]);
        (Boston, Map.ofList [(StLouis, 986)]);
        (NewYork, Map.ofList [(Boston, 211)])
    ]
    |> transposeCombine

let shortestPathBetween startCity endCity =
  let rec searchForShortestPath currentCity distanceSoFar citiesVisitedSoFar accMap =
    let visitDestinations m =
      (m, distanceBetweenCities.[currentCity])
        ||> Map.fold
          (fun acc city distance ->
             searchForShortestPath city (distance + distanceSoFar) (citiesVisitedSoFar @ [city]) acc)

    match Map.tryFind currentCity accMap with
    | None -> accMap |> Map.add currentCity (distanceSoFar, citiesVisitedSoFar) |> visitDestinations
    | Some x ->
        let (shortestKnownPath, _) = x
        if distanceSoFar < shortestKnownPath then
          accMap |> Map.add currentCity (distanceSoFar, citiesVisitedSoFar) |> visitDestinations
        else accMap

  let shortestPaths = searchForShortestPath startCity 0 [] Map.empty
  shortestPaths.[endCity]

shortestPathBetween LosAngeles NewYork //(2721, [Denver; StLouis; Chicago; NewYork])
