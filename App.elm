import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Task

type Route = SelectAdventure | PlayAdventure Adventure

type alias AdventureLink =
  { title : String
  , url: String
  }

type alias Adventure =
  { title : String
  }

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL
type alias Model =
  { adventureLinks : List AdventureLink
  , route : Route
  }

init : (Model, Cmd Msg)
init =
  ( Model [] SelectAdventure
  , getAdventureLinks
  )


-- UPDATE
type Msg
  = MorePlease
  | FetchAdventureLinksSucceed (List AdventureLink)
  | FetchFail Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchAdventureLinksSucceed adventureLinks ->
      (Model adventureLinks SelectAdventure, Cmd.none)

    FetchFail _ ->
      (model, Cmd.none)

    MorePlease ->
      (model, getAdventureLinks)


-- VIEW
view : Model -> Html Msg
view model =
  div []
    (List.map (\a -> div[] [ text a.title ]) model.adventureLinks)


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- HTTP
getAdventureLinks : Cmd Msg
getAdventureLinks =
  let
    url =
      "http://localhost:8000/adventures.json"
  in
    Task.perform FetchFail FetchAdventureLinksSucceed (Http.get decodeAdventureLinks url)


-- DECODERS
decodeAdventureLink : Json.Decoder AdventureLink
decodeAdventureLink =
  Json.object2 AdventureLink (Json.at ["title"] Json.string) (Json.at ["url"] Json.string)

decodeAdventureLinks : Json.Decoder (List AdventureLink)
decodeAdventureLinks =
  Json.at ["adventures"] (Json.list decodeAdventureLink)
