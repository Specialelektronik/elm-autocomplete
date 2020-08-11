module RemoteSource exposing (main)

import Autocomplete
import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes as Attributes
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Task exposing (Task)


autocompleteConfig : Autocomplete.Config Country Msg
autocompleteConfig =
    { transform = AutocompleteMsg
    , fetch = fetch
    , submit = \_ -> NoOp
    , chose = SelectedCountry
    , focus = NoOp
    }


type alias Country =
    { name : String
    , flag : String
    , region : String
    }


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { autocomplete : Autocomplete.Autocomplete Country
    , selected : Maybe Country
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
    | CompletedLoadCountries (Result Http.Error (List Country))
    | SelectedCountry Country
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

        CompletedLoadCountries (Ok result) ->
            ( { model | autocomplete = Autocomplete.setSuggestions result model.autocomplete, error = "" }, Cmd.none )

        CompletedLoadCountries (Err err) ->
            ( { model | error = errorToString err, autocomplete = Autocomplete.setSuggestions [] model.autocomplete }, Cmd.none )

        SelectedCountry country ->
            ( { model | selected = Just country }, Cmd.none )


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "BadUrl " ++ url

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadStatus statusCode ->
            "BadStatus : " ++ String.fromInt statusCode

        Http.BadBody body ->
            "BadBody : " ++ body


view : Model -> Html Msg
view model =
    let
        { suggestions, index } =
            Autocomplete.toView model.autocomplete

        help =
            "Search for a country (Source: https://restcountries.eu/). Use arrow up and down to select a country and Enter to choose."
    in
    div []
        [ Html.p [] [ Html.text help ]
        , Autocomplete.input autocompleteConfig [] model.autocomplete
        , Html.ul [] (List.indexedMap (viewSuggestion index) suggestions)
        , Html.p [] [ Html.strong [] [ Html.text model.error ] ]
        , Html.p [] [ Html.em [] [ Html.text (viewSelected model.selected) ] ]
        ]


viewSuggestion : Maybe Int -> Int -> Country -> Html Msg
viewSuggestion maybeSelectedIndex index country =
    let
        name =
            country.name

        text =
            Maybe.map
                (\selectedIndex ->
                    if selectedIndex == index then
                        "► " ++ name

                    else
                        name
                )
                maybeSelectedIndex
                |> Maybe.withDefault name
    in
    Html.li [
        Attributes.style "display" "flex"
        , Attributes.style "align-items" "center"
        , Attributes.style "margin" "4px"
    ]
        [ Html.img [ Attributes.src country.flag, Attributes.style "width" "40px", Attributes.style "height" "auto", Attributes.style "margin-right" "8px" ] []
        , Html.text text
        , Html.em [] [ Html.text (" (" ++ country.region ++ ")") ]
        ]


viewSelected : Maybe Country -> String
viewSelected maybeSelected =
    case maybeSelected of
        Nothing ->
            "No country selected"

        Just selected ->
            "Selected: " ++ selected.name


fetch : String -> Cmd Msg
fetch query =
    Http.get
        { url = "https://restcountries.eu/rest/v2/name/" ++ query
        , expect = Http.expectJson CompletedLoadCountries (Decode.list decoder)
        }


decoder : Decoder Country
decoder =
    Decode.map3 Country
        (Decode.field "name" Decode.string)
        (Decode.field "flag" Decode.string)
        (Decode.field "region" Decode.string)
