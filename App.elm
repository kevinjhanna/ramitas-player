import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Task
import Dict


type Route = ListingAdventures | PlayingAdventure Adventure

type alias AdventureLink =
  { title : String
  , url: String
  }

type alias Adventure =
  { title : String
  , events: Dict.Dict String Event
  , currentEvent: Maybe Event
  , startingEvent: Maybe Event
  }

type alias Action =
  { description: String
  , outputs: List String
  }

type alias Event =
  { title: String
  -- , description: String
  , actions: List Action
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
  ( Model [] ListingAdventures
  , getAdventureLinks
  )


-- UPDATE
type Msg
  = SelectAdventure AdventureLink
  | FetchAdventureLinksSucceed (List AdventureLink)
  | FetchAdventureSucceed Adventure
  | FetchFail Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchAdventureLinksSucceed adventureLinks ->
      (Model adventureLinks ListingAdventures, Cmd.none)

    FetchFail _ ->
      (model, Cmd.none)

    SelectAdventure adventureLink ->
      (model, getAdventure adventureLink.url)

    FetchAdventureSucceed adventure ->
      let
        newModel = { model | route = PlayingAdventure adventure}
      in
        (newModel, Cmd.none)


-- VIEW
view : Model -> Html Msg
view model =
  case model.route of
    ListingAdventures -> viewAdventureLinks model.adventureLinks
    PlayingAdventure adventure -> viewAdventure adventure

viewAdventureLink : AdventureLink -> Html Msg
viewAdventureLink adventureLink = div
  [
    onClick (SelectAdventure adventureLink)
  ]
  [ text adventureLink.title
  ]

viewAdventureLinks : List AdventureLink -> Html Msg
viewAdventureLinks adventureLinks =
  div []
    (List.map viewAdventureLink adventureLinks)

viewAdventure : Adventure -> Html Msg
viewAdventure adventure = div []
  [ h1 [] [text adventure.title]
  , viewEvent adventure.currentEvent
  ]

viewEvent : Maybe Event -> Html Msg
viewEvent maybe =
  case maybe of
    Just event -> div []
      [ text event.title
      , div [] (List.map (\a -> text a.description) event.actions)
      ]
    Nothing -> text "Dead end."


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

getAdventure : String -> Cmd Msg
getAdventure url =
    Task.perform FetchFail FetchAdventureSucceed (Http.get decodeAdventure url)


-- DECODERS
decodeAction : Json.Decoder Action
decodeAction = Json.object2 Action
  (Json.at ["description"] Json.string)
  (Json.at ["outputs"] (Json.list Json.string))

decodeEvent : Json.Decoder Event
decodeEvent = Json.object2 Event
  (Json.at ["title"] Json.string)
  (Json.at ["actions"] (Json.list decodeAction))

loadAdventure : String -> String -> (Dict.Dict String Event) -> Adventure
loadAdventure title startingEventId events =
  let
    startingEvent = Dict.get startingEventId events
  in
    { title = title, startingEvent = startingEvent, events = events, currentEvent = startingEvent }

decodeAdventure : Json.Decoder Adventure
decodeAdventure =
  Json.object3 loadAdventure
    (Json.at ["title"] Json.string)
    (Json.at ["startingEventId"] Json.string)
    (Json.at ["events"] (Json.dict decodeEvent))

decodeAdventureLink : Json.Decoder AdventureLink
decodeAdventureLink =
  Json.object2 AdventureLink (Json.at ["title"] Json.string) (Json.at ["url"] Json.string)

decodeAdventureLinks : Json.Decoder (List AdventureLink)
decodeAdventureLinks =
  Json.at ["adventures"] (Json.list decodeAdventureLink)
