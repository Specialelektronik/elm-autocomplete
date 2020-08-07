module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Autocomplete
import Task exposing (Task)

autocompleteConfig : Autocomplete.Config City Msg
autocompleteConfig =
    {
        transform = AutocompleteMsg
    , fetch = fetch
    , submit = (\_ -> NoOp)
    , chose = (\_ -> NoOp)
    , focus = NoOp
    }

type alias City = String

main =
  Browser.element { init = init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }

type alias Model = {
    autocomplete : Autocomplete.Autocomplete City
    }

init : () -> (Model, Cmd Msg)
init flags =
    ({ autocomplete = Autocomplete.init ""}, Cmd.none)

type Msg = AutocompleteMsg Autocomplete.Msg
    | CompletedLoadCities (List City)
    | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
        AutocompleteMsg subMsg ->
            let
                { newAutocomplete, maybeMsg, cmd } =
                    Autocomplete.update (autocompleteConfig) subMsg model.autocomplete

                newModel =
                    { model | autocomplete = newAutocomplete }
            in
            case maybeMsg of
                Nothing ->
                    ( newModel, cmd )

                Just msg_ ->
                    update msg_ newModel
        CompletedLoadCities cities ->
            ( { model | autocomplete = Autocomplete.setSuggestions cities model.autocomplete }, Cmd.none )
        NoOp -> (model, Cmd.none)


view model =
    let
        {suggestions } = Autocomplete.toView model.autocomplete
    in
  div []
    [ Autocomplete.input autocompleteConfig [] model.autocomplete
    , Html.p [] [ Html.text (Debug.toString suggestions) ]
    ]

fetch : String -> Cmd Msg
fetch query =
    Task.perform CompletedLoadCities (Task.succeed ["Karlstad", "Karlshamn", "Karlslund"])