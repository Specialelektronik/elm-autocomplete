module Simple exposing (main)

import Autocomplete
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Task exposing (Task)


cities : List String
cities =
    [ "Karlstad", "Karlshamn", "Karlslund", "Oskarshamn", "Stockholm" ]


autocompleteConfig : Autocomplete.Config City Msg
autocompleteConfig =
    { transform = AutocompleteMsg
    , fetch = fetch
    , submit = \_ -> NoOp
    , chose = SelectedCity
    , focus = NoOp
    }


type alias City =
    String


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { autocomplete : Autocomplete.Autocomplete City
    , selected : Maybe City
    , error : String
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { autocomplete = Autocomplete.init ""
      , selected = Nothing
      , error = ""
      }
    , Cmd.none
    )


type Msg
    = AutocompleteMsg Autocomplete.Msg
    | CompletedLoadCities (Result String (List City))
    | SelectedCity City
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AutocompleteMsg subMsg ->
            let
                { newAutocomplete, maybeMsg, cmd } =
                    Autocomplete.update autocompleteConfig subMsg model.autocomplete

                newModel =
                    { model | autocomplete = newAutocomplete }
            in
            case maybeMsg of
                Nothing ->
                    ( newModel, cmd )

                Just msg_ ->
                    update msg_ newModel

        CompletedLoadCities (Ok result) ->
            ( { model | autocomplete = Autocomplete.setSuggestions result model.autocomplete, error = "" }, Cmd.none )

        CompletedLoadCities (Err errMsg) ->
            ( { model | error = errMsg, autocomplete = Autocomplete.setSuggestions [] model.autocomplete }, Cmd.none )

        SelectedCity city ->
            ( { model | selected = Just city }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        { suggestions, index } =
            Autocomplete.toView model.autocomplete

        help =
            "Search for a city (available cities): [ " ++ String.join ", " cities ++ " ] . Use arrow up and down to select a city and Enter to choose."
    in
    div []
        [ Html.p [] [ Html.text help ]
        , Autocomplete.input autocompleteConfig [] model.autocomplete
        , Html.ul [] (List.indexedMap (viewSuggestion index) suggestions)
        , Html.p [] [ Html.strong [] [ Html.text model.error ] ]
        , Html.p [] [ Html.em [] [ Html.text (viewSelected model.selected) ] ]
        ]


viewSuggestion : Maybe Int -> Int -> City -> Html Msg
viewSuggestion maybeSelectedIndex index city =
    let
        text =
            Maybe.map
                (\selectedIndex ->
                    if selectedIndex == index then
                        "► " ++ city

                    else
                        city
                )
                maybeSelectedIndex
                |> Maybe.withDefault city
    in
    Html.li []
        [ Html.text text
        ]


viewSelected : Maybe City -> String
viewSelected maybeSelected =
    case maybeSelected of
        Nothing ->
            "No city selected"

        Just selected ->
            "Selected: " ++ selected


fetch : String -> Cmd Msg
fetch query =
    cities
        |> List.filter (String.startsWith query)
        |> (\list ->
                if List.isEmpty list then
                    Task.fail "No cities found."

                else
                    Task.succeed list
           )
        |> Task.attempt CompletedLoadCities
