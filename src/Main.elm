module Main exposing (..)

import Browser
import Html exposing (Html)
import Http
import Unison.GitHub


type alias Model =
    { result : Maybe (Result Http.Error Unison.GitHub.RepoContents)
    }


type Msg
    = Msg (Result Http.Error Unison.GitHub.RepoContents)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { result = Nothing }
    , Cmd.map Msg (Unison.GitHub.getRepoContents "unisonweb" "unisonbase" ".unison/v1")
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg result ->
            ( { model | result = Just result }, Cmd.none )


view : Model -> Html Msg
view model =
    Html.text <| Debug.toString model
