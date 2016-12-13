module Columns exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

import Types exposing (..)




-- MODEL

defaultColumns =
  [[[Card "0" "default"]]]




-- UPDATE

type ColumnMsg
  = NoOp


update : ColumnMsg -> List Column -> List Column
update msg cols =
  case msg of
    NoOp ->
      cols




-- VIEW

view : ViewState -> List Column -> Html Msg
view vstate cols =
  div [ id "app"
      ]
      [text "here"]




-- HELPERS

getContent : String -> List Column -> String
getContent id cols =
  "implement getContent"
