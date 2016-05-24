module Models

type Pos =
    { x: int
      y: int }

type Hero =
    { pos: Pos
      life: int
      spawn: Pos
      crashed: bool }

type Move =
    | Stay
    | North
    | East
    | South
    | West

type MapElement =
    | Free
    | Wood
    | Tavern
    | GoldMe
    | GoldTaken
    | Hero

type Map =
    { dim: int
      tiles: MapElement[,] }

type State = 
    { id: int
      finished: bool      
      pos: Pos
      spawnPos: Pos
      map: Map
      gold: int
      maxGold: int
      playUrl: string
      showUrl: string
      turn: int }