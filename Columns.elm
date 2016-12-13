module Columns exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onDoubleClick)
import Markdown exposing (toHtmlWith)

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
  let
    viewablestate =
      VisibleViewState
        vstate.active
        vstate.editing
        vstate.descendants

    viewCols =
      [[[]]] ++
      cols ++
      [[[]]]
  in
  div [ id "app"
      ]
      (List.indexedMap (viewColumn viewablestate) viewCols)


viewColumn : VisibleViewState -> Int -> Column -> Html Msg
viewColumn vstate depth col =
  let
    buffer =
      [div [ class "buffer" ][]]
  in
  div
    [ class "column" ]
    ( buffer ++
      (List.map (viewGroup vstate depth) col) ++
      buffer
    )


viewGroup : VisibleViewState -> Int -> Group -> Html Msg
viewGroup vstate depth cards =
  div
    [ classList [ ("group", True)
                ]
    ]
    (List.map viewCard cards)


viewCard : Card -> Html Msg
viewCard card =
  let
    options =
      { githubFlavored = Just { tables = True, breaks = True }
      , defaultHighlighting = Nothing
      , sanitize = False
      , smartypants = False
      }
  in
  div [ id ("card-" ++ card.id)
      , classList [ ("card", True)
                  ]
      ]
      [ Markdown.toHtmlWith options
          [ class "view"
          , onClick (Activate card.id)
          , onDoubleClick (OpenCard card.id card.content)
          ] 
          card.content
      ]




-- HELPERS

getContent : String -> List Column -> String
getContent id cols =
  "implement getContent"
