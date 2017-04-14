module Types exposing (..)




type Msg
    = NoOp
    -- === Card Activation ===
    | Activate String
    | GoLeft String
    | GoDown String
    | GoUp String
    | GoRight String
    -- === Card Editing  ===
    | OpenCard String String
    | GetContentToSave String
    | AttemptSaveContent (String, String)
    | UpdateCard (String, String)
    | UpdateCardError String
    | DeleteCard String
    | CancelCard
    -- === Card Insertion  ===
    | Insert TreeNode String Int
    | InsertAbove String
    | InsertBelow String
    | InsertChild String
    -- === Card Moving  ===
    | Move String String Int
    | MoveUp String
    | MoveDown String
    | MoveLeft String
    | MoveRight String
    -- === Ports ===
    | AttemptSave
    | SaveResponses (List Response)
    | HandleKey String



-- OBJECTS


type alias Tree =
  { id : String
  , content : String
  , children : Children
  , rev : Maybe String
  }


type alias TreeNode =
  { content : String
  , children : List (String, Bool)
  , rev : Maybe String
  }


type alias Response =
  { id : String
  , rev : String
  , ok : Bool
  }


type Children = Children (List Tree)
type alias Group = List Tree
type alias Column = List (List Tree)




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
