module Columns exposing (..)

import List.Extra exposing (getAt, findIndex, (!!))
import Tuple exposing (first, second)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onDoubleClick)
import Markdown exposing (toHtmlWith)

import Types exposing (..)




-- MODEL

defaultColumns =
  [ [ [ Card "0" "Root"]]
  , [ [ Card "1" "A"
      , Card "2" "B"
      ]
    ]
  , [ [ Card "3" "A1"
      , Card "5" "A2"
      ]
    , [ Card "4" "B1"]
    ]
  , [ []
    , []
    , []
    ]
  ]




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
      cols
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
  let
    viewFunction c =
      let
        isActive =
          c.id == vstate.active

        isEditing =
          case vstate.editing of
            Just editId ->
              c.id == editId

            Nothing ->
              False
      in
      viewCard (isActive, isEditing, depth) c
  in
  div
    [ classList [ ("group", True)
                ]
    ]
    (List.map viewFunction cards)


viewCard : (Bool, Bool, Int) -> Card -> Html Msg
viewCard (isActive, isEditing, depth) card =
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
                  , ("active", isActive)
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


getActiveGroups : (Int, Int) -> List Column -> List (List Bool)
getActiveGroups (aci, afi) cols =
  let
    inactiveGroups =
      List.take (aci+1) cols
        |> List.map (\col -> List.map (\_ -> False) col)
        |> Debug.log "inactiveGroups"

    activeCols =
      List.drop (aci+1) cols

    nextCol : Column -> List (Bool, Int) -> List (Bool, Int)
    nextCol col prevState =
      prevState
        |> List.map (\s -> List.repeat (second s) (first s))
        |> List.concat
        |> List.map2 (\g b -> (b, List.length g)) col

    firstActiveState =
      List.take 1 activeCols
        |> List.indexedMap (\i g -> (i == afi, List.length g))

    activeGroups =
      List.scanl nextCol firstActiveState activeCols
        |> List.map (\c -> List.map (\g -> first g) c)
        |> Debug.log "activeGroups"
  in
  inactiveGroups ++ activeGroups


getCoords : String -> List Column -> Maybe Coords
getCoords id cols =
  let
    allIds =
      cols
        |> List.map (\c -> List.concat c)
        |> List.concat
        |> List.map .id
  in
  if List.member id allIds then
    let
      colIdx_ =
        getColumnIndex id cols
          |> Debug.log "colIdx_"

      col_ =
        colIdx_
          |> Maybe.andThen (\ci -> cols !! ci)

      groupIdx_ =
        col_
          |> Maybe.andThen (\col -> getGroupIndex id col)
          |> Debug.log "groupIdx_"

      group_ =
        case (groupIdx_, col_) of
          (Just groupIdx, Just col) ->
            col !! groupIdx

          _ -> Nothing

      cardIdx_ =
        group_
          |> Maybe.andThen (\g -> getCardIndex id g)

      card_ =
        case (cardIdx_, group_) of
          (Just cardIdx, Just group) ->
            group !! cardIdx

          _ -> Nothing

      flatIdx_ =
        col_
          |> Maybe.andThen (\c -> getFlatIndex id c)

    in
    case (colIdx_, groupIdx_, cardIdx_, flatIdx_) of
      (Just c, Just g, Just i, Just f) ->
        Just (Coords c g i f)

      x -> 
        let db1 = Debug.log "coords" x in
        Nothing 
  else
    Nothing


getColumnIndex : String -> List Column -> Maybe Int
getColumnIndex id cols =
  findIndex 
    (\c -> List.member id <| List.map .id <| c) 
    (cols |> List.map List.concat)


getGroupIndex : String -> Column -> Maybe Int
getGroupIndex id col =
  findIndex
    (\g -> List.member id <| List.map .id <| g)
    col


getCardIndex : String -> Group -> Maybe Int
getCardIndex id g =
  findIndex
    (\cd -> .id cd == id)
    g


getFlatIndex : String -> Column -> Maybe Int
getFlatIndex id col =
  findIndex
    (\cd -> .id cd == id)
    (col |> List.concat)




-- GENERAL HELPERS

(?) : Maybe a -> a -> a
(?) maybe default =
  Maybe.withDefault default maybe

infixr 9 ?
