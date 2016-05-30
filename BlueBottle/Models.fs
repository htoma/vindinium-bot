module Models

type Pos =
    { x: int
      y: int }

type Hero =
    { id: int 
      elo: int
      pos: Pos
      spawn: Pos
      life: int
      mines: int
      gold: int }

type Move =
    | Stay
    | North
    | East
    | South
    | West

type Action =
    | NoAction
    | Fight
    | Drink
    | Mine
    | LookupDrink
    | LookupMine

type MapElement =
    | Free
    | Wood
    | Tavern
    | GoldF
    | GoldHero of int
    | Hero

type Map =
    { dim: int
      tiles: MapElement[,] }

type State = 
    { finished: bool
      me: Hero
      heroes: Hero list      
      map: Map
      playUrl: string
      showUrl: string
      turn: int }
      with 
      member this.maxGold() = 
        this.heroes
        |> List.map (fun h -> h.gold)
        |> List.max