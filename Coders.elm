module Coders exposing (..)

import Types exposing (..)
import Json.Encode
import Json.Decode as Json exposing (..)
import Array exposing (fromList)
import String


type alias Model =
  { columns : List Column
  , viewState : ViewState
  , nextId : Int
  , saved : Bool
  }


-- ENCODERS

modelToValue : Model -> Json.Encode.Value
modelToValue model =
  Json.Encode.object
   [ ("columns", Json.Encode.list (List.map columnToValue model.columns))
   , ("viewState", viewStateToValue model.viewState)
   , ("nextId", Json.Encode.int model.nextId)
   ]


columnToValue : Column -> Json.Encode.Value
columnToValue col =
  Json.Encode.object
    [ ("test", Json.Encode.string "test" )
    ]



viewStateToValue : ViewState -> Json.Encode.Value
viewStateToValue vs =
  Json.Encode.object
    [ ( "active", Json.Encode.string vs.active )
    , ( "activePast", Json.Encode.list (List.map Json.Encode.string vs.activePast) )
    , ( "activeFuture", Json.Encode.list (List.map Json.Encode.string vs.activeFuture) )
    , ( "descendants", Json.Encode.list (List.map Json.Encode.string vs.descendants) )
    , ( "editing", maybeToValue vs.editing Json.Encode.string )
    ]






-- DECODERS

modelDecoder : Decoder Model
modelDecoder =
  Json.map4 Model
    (field "columns" (list (list (list cardDecoder))))
    (field "viewState" viewStateDecoder)
    (field "nextId" int)
    ( succeed True )


cardDecoder : Decoder Card
cardDecoder =
  Json.map2 Card
    (succeed "testId")
    (succeed "testContent")



viewStateDecoder : Decoder ViewState
viewStateDecoder =
  Json.map5 ViewState
    (field "active" string)
    (field "activePast" (list string))
    (field "activeFuture" (list string))
    (field "descendants" (list string))
    (maybe (field "editing" string))
    
  



-- HELPERS


maybeToValue : Maybe a -> (a -> Json.Encode.Value) -> Json.Encode.Value
maybeToValue mb encoder =
  case mb of
    Nothing -> Json.Encode.null
    Just v -> encoder v
