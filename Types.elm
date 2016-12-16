module Types exposing (..)

import Json.Decode as Json



type Msg
    = NoOp
    -- === Card Activation ===
    | Activate String
    | ActivatePast
    | ActivateFuture
    | GoLeft String
    | GoDown String
    | GoUp String
    | GoRight String
    -- === Card Editing  ===
    | OpenCard String String
    | AttemptUpdateCard String
    | UpdateCard (String, String)
    | UpdateCardError String
    | DeleteCard String
    | CancelCard
    -- === Card Insertion  ===
--  | Insert Tree String Int
--  | InsertAbove String
--  | InsertBelow String
--  | InsertChild String
    -- === Card Moving  ===
--  | Move Tree String Int
--  | MoveUp String
--  | MoveDown String
--  | MoveLeft String
--  | MoveRight String
    -- === History ===
--  | Undo
--  | Redo
--  | AddToUndo Tree
    -- === Ports ===
--  | SaveTemp
--  | Confirm String String String
    | DataIn Json.Value
    | ExternalCommand (String, String)
    | HandleKey String




-- TRANSIENTS

type alias ViewState =
  { active : String
  , activePast : List String
  , activeFuture : List String
  , descendants : List String
  , editing : Maybe String
  }


type alias VisibleViewState =
  { active : String
  , editing : Maybe String
  , descendants : List String
  }


type alias Card =
  { id : String
  , content : String
  }


type alias Group = List Card
type alias Column = List (List Card)


type alias Coords =
  { column : Int
  , group : Int
  , index : Int
  , flat: Int
  }
