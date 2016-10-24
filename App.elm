import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Task
import Dict
import Random
import Array

type Route = ListingAdventures | PlayingAdventure

type alias AdventureLink =
  { title : String
  , url: String
  }

type alias Adventure =
  { title : String
  , events: Dict.Dict String Event
  -- , currentEvent: Maybe Event
  , startingEvent: Maybe Event
  }

type alias Action =
  { description: String
  , outputs: List String
  }

type alias Event =
  { title: String
  , description: String
  , actions: List Action
  }

emptyEvent : Event
emptyEvent =
  { title = "Empty event"
  , actions = []
  , description = ""
  }

emptyAdventure : Adventure
emptyAdventure =
  { title = ""
  , events = Dict.empty
  , startingEvent = Just emptyEvent
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
  , selectedAdventure : Maybe Adventure
  , currentEvent : Maybe Event
  }

init : (Model, Cmd Msg)
init =
  let
    model =
      { adventureLinks = []
      , route = ListingAdventures
      , selectedAdventure = Nothing
      , currentEvent = Nothing
      }
  in
    (model, getAdventureLinks)

-- UPDATE
type Msg
  = SelectAdventure AdventureLink
  | FetchAdventureLinksSucceed (List AdventureLink)
  | FetchAdventureSucceed Adventure
  | FetchFail Http.Error
  | RollAction Action
  | SetEvent (Array.Array String) Int
  | ListAdventureLinks


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchAdventureLinksSucceed adventureLinks ->
      let
        newModel = { model | adventureLinks = adventureLinks }
      in
        (newModel, Cmd.none)

    FetchFail _ ->
      (model, Cmd.none)

    SelectAdventure adventureLink ->
      (model, getAdventure adventureLink.url)

    FetchAdventureSucceed adventure ->
      let
        newModel = { model
          | route = PlayingAdventure
          , selectedAdventure = Just adventure
          , currentEvent = adventure.startingEvent
        }
      in
        (newModel, Cmd.none)

    SetEvent eventIds randomIndex ->
      case model.selectedAdventure of
        Nothing -> (model, Cmd.none)
        Just adventure ->
          let
            maybeEventId = Array.get randomIndex eventIds
            eventId = Maybe.withDefault "1" maybeEventId
            maybeEvent = Dict.get eventId adventure.events
            newModel = { model | currentEvent = maybeEvent }
          in
            (newModel, Cmd.none)

    RollAction action ->
      let
        array = Array.fromList action.outputs
        length = Array.length array
        randomIndex = Random.int 0 (length - 1)
        cmd = Random.generate (SetEvent array) randomIndex
      in
        (model, cmd)

    ListAdventureLinks ->
      let
        newModel = { model | route = ListingAdventures}
      in
        (newModel, Cmd.none)


-- VIEW
view : Model -> Html Msg
view model =
  div []
  [ viewNavbar
  , div
    [  ]
    [ case model.route of
        ListingAdventures -> viewAdventureLinks model.adventureLinks
        PlayingAdventure -> viewAdventure model.selectedAdventure model.currentEvent
    ]
  ]

viewNavbar : Html Msg
viewNavbar = div
  [ class "navbar"
  ]
  [ a
    [ class "navbar-logo"
    , onClick ListAdventureLinks
    ]
    [ text "Ramitas"
    ]
  ]

viewAdventureLink : AdventureLink -> Html Msg
viewAdventureLink adventureLink = a
  [ onClick (SelectAdventure adventureLink)
  ]
  [ text adventureLink.title
  ]

viewAdventureLinks : List AdventureLink -> Html Msg
viewAdventureLinks adventureLinks =
  let
    row = (\element -> tr [] [ td [] [ element ]])
  in
    div
    [
    ]
    [ div
      [ class "splash-image"
      ]
      [ div
        [ class "grid"
        ]
        [ h1
          [ class "splash-title"
          ]
          [ text "Aventuras"
          ]
        ]
      ]
    , div
      [ class "grid"
      ]
      [ table
        [ class "table table-striped"
        ]
        [ tbody [] (List.map (\link -> row (viewAdventureLink link)) adventureLinks)
        ]
      ]
    ]

viewAdventure : Maybe Adventure -> Maybe Event -> Html Msg
viewAdventure maybeAdventure maybeCurrentEvent =
  case maybeAdventure of
    Nothing -> div [] []
    Just adventure -> div
      [ class "grid adventure"
      ]
      [ h1
        [
        ]
        [ text adventure.title
        ]
      , viewEvent maybeCurrentEvent
      ]

viewAction : Action -> Html Msg
viewAction action = div []
  [ a
    [ onClick (RollAction action)
    , class "btn btn-outline"
    ]
    [ text action.description
    ]
  ]

viewEvent : Maybe Event -> Html Msg
viewEvent maybe =
  case maybe of
    Just event -> div []
      [ strong [] [ text event.title ]
      , p [] [ text event.description ]
      , div [] (List.map viewAction event.actions)
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
decodeEvent = Json.object3 Event
  (Json.at ["title"] Json.string)
  (Json.at ["description"] Json.string)
  (Json.at ["actions"] (Json.list decodeAction))

loadAdventure : String -> String -> (Dict.Dict String Event) -> Adventure
loadAdventure title startingEventId events =
  { title = title
  , startingEvent = Dict.get startingEventId events
  , events = events
  }

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
