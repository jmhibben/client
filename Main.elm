port module Main exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import String
import Tuple exposing (first, second)
import Json.Encode
import Json.Decode as Json
import Dom
import Task
import Markdown
import List.Extra as ListExtra

import Types exposing (..)
import Columns exposing (update, view, defaultColumns, getContent)
import Coders exposing (modelDecoder, modelToValue)


main : Program Json.Value Model Msg
main =
  programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


port activateCards : (Int, List (List String)) -> Cmd msg
port attemptUpdate : String -> Cmd msg
port message : (String, Json.Encode.Value) -> Cmd msg


-- MODEL


type alias Model =
  { columns : List Column
  , viewState : ViewState
  , nextId : Int
  , saved : Bool
  }


defaultModel : Model
defaultModel =
  { columns = [[[]]]
  , viewState = 
      { active = "0"
      , activePast = []
      , activeFuture = []
      , descendants = []
      , editing = Just "0"
      }
  , nextId = 1
  , saved = True
  }


init : Json.Value -> (Model, Cmd Msg)
init savedState =
  case Json.decodeValue modelDecoder savedState of
    Ok model ->
      model 
        ! [ focus model.viewState.active
          ]
    Err err ->
      let
        deb = Debug.log "init decode error" err
      in
      defaultModel 
        ! [ focus defaultModel.viewState.active
          ]


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    vs = model.viewState
  in
  case msg of
    NoOp ->
      model ! []

    -- === Card Activation ===

    Activate id ->
      model ! []

    GoLeft id ->
      model ! []

    GoDown id ->
      model ! []

    GoUp id ->
      model ! []

    GoRight id ->
      model ! []
      
    -- === Card Editing  ===

    OpenCard id str ->
      { model 
        | viewState = { vs | active = id, editing = Just id }
      } 
        ! [ focus id ]

    AttemptUpdateCard id ->
      let
        tree_ =
          Nothing
          -- TODO: getTree id model.tree
      in
      case tree_ of
        Just tree ->
          model ! [attemptUpdate id]

        Nothing ->
          model ! []
            |> andThen (UpdateCardError "Elm error: Card not found in tree.")

    UpdateCard (id, str) ->
      { model
        | columns = Columns.update (Columns.NoOp) model.columns
        , viewState = { vs | active = id, editing = Nothing }
      }
        ! [] 

    UpdateCardError err ->
      Debug.crash err
    
    DeleteCard id ->
      model ! []

    CancelCard ->
      { model 
        | viewState = { vs | editing = Nothing }
      } 
        ! []


    -- === Card Insertion  ===
    -- === Card Moving  ===
    -- === History ===
    -- === Ports ===

    ExternalCommand (cmd, arg) ->
      case cmd of
        "keyboard" ->
          model ! [run (HandleKey arg)]

        "confirm-cancel" ->
          if arg == "true" then
            update CancelCard model
          else
            model ! []

        _ ->
          let
            db1 = Debug.log "Unknown external command" cmd
          in
          model ! []

    DataIn json ->
      init json

    HandleKey str ->
      let
        vs = model.viewState
      in
      case str of
        "mod+x" ->
          let
            db1 = Debug.log "model" model
          in
          model ! []

        "mod+s" ->
          model ! [ message ("save", modelToValue model) ]

        "mod+enter" ->
          editMode model
            (\id -> AttemptUpdateCard id)

        "enter" ->
          normalMode model
            (OpenCard vs.active (getContent vs.active model.columns))

        "esc" ->
          update CancelCard model -- TODO

        "mod+backspace" ->
          normalMode model
            (DeleteCard vs.active)

        "mod+j" ->
          update (NoOp) model

        "mod+down" ->
          normalMode model
            (NoOp)

        "mod+k" ->
          update (NoOp) model

        "mod+up" ->
          normalMode model
            (NoOp)

        "mod+l" ->
          update (NoOp) model

        "mod+right" ->
          normalMode model
            (NoOp)

        "h" ->
          normalMode model
            (GoLeft vs.active)

        "left" ->
          normalMode model
            (GoLeft vs.active)

        "j" ->
          normalMode model
            (GoDown vs.active)

        "down" ->
          normalMode model
            (GoDown vs.active)

        "k" ->
          normalMode model
            (GoUp vs.active)
  
        "up" ->
          normalMode model
            (GoUp vs.active)
  
        "l" ->
          normalMode model
            (GoRight vs.active)

        "right" ->
          normalMode model
            (GoRight vs.active)

        "alt+up" ->
          normalMode model
            (NoOp)

        "alt+down" ->
          normalMode model
            (NoOp)

        "alt+left" ->
          normalMode model
            (NoOp)

        "alt+right" ->
          normalMode model
            (NoOp)

        "mod+z" ->
          normalMode model NoOp

        "mod+r" ->
          normalMode model NoOp

        "[" ->
          normalMode model NoOp

        "]" ->
          normalMode model NoOp

        other ->
          let
            deb = Debug.log "keyboard" other
          in
          model ! []

    _ ->
      model ! []


andThen : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
andThen msg (model, prevMsg) =
  let
    newStep =
      update msg model
  in
  ( first newStep, Cmd.batch [prevMsg, second newStep] )


onlyIf : Bool -> Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
onlyIf cond msg prevStep =
  if cond then
    prevStep
      |> andThen msg
  else
    prevStep




-- VIEW


view : Model -> Html Msg
view model =
  (lazy2 Columns.view model.viewState model.columns)




-- SUBSCRIPTIONS

port externals : ((String, String) -> msg) -> Sub msg -- ~ Sub (String, String)
port updateSuccess : ((String, String) -> msg) -> Sub msg
port updateError : (String -> msg) -> Sub msg
port data : (Json.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ externals ExternalCommand
    , updateSuccess UpdateCard
    , updateError UpdateCardError
    , data DataIn
    ]




-- HELPERS

focus : String -> Cmd Msg
focus id =
  Task.attempt (\_ -> NoOp) (Dom.focus ("card-edit-" ++ id))


run : Msg -> Cmd Msg
run msg =
  Task.attempt (\_ -> msg ) (Task.succeed msg)


editMode : Model -> (String -> Msg) -> (Model, Cmd Msg)
editMode model editing = 
  case model.viewState.editing of
    Nothing ->
      model ! []

    Just uid ->
      update (editing uid) model


normalMode : Model -> Msg -> (Model, Cmd Msg)
normalMode model msg = 
  case model.viewState.editing of
    Nothing ->
      update msg model

    Just _ ->
      model ! []
