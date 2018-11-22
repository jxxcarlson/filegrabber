module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, pre)
import Html.Events exposing (onClick)
import Bytes exposing (..)
import Http
import FileGrabber
import File.Download as Download


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


type alias Model =
    { status : String
    , maybeBytes : Maybe Bytes
    }


initialModel : Model
initialModel =
    { status = "Starting up"
    , maybeBytes = Nothing
    }


imageUrl =
    "https://natgeo.imgix.net/factsheets/thumbnails/01-frog-day-gallery.adapt.1900.1.jpg?auto=compress,format&w=1024&h=560&fit=crop"


type Msg
    = GetData
    | GotData (Result Http.Error Bytes)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetData ->
            ( model, getData imageUrl )

        GotData result ->
            case result of
                Ok data ->
                    ( { model
                        | status =
                            "Bytes received = " ++ (String.fromInt (Bytes.width data))
                        , maybeBytes = Just data
                      }
                    , saveData data
                    )

                Err _ ->
                    ( { model | status = "Invalid data" }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick GetData ] [ text "Get image" ]
        , pre [] [ text <| "status: " ++ model.status ]
        ]


getData : String -> Cmd Msg
getData url =
    Http.get
        { url = url
        , expect = FileGrabber.expectBytes GotData
        }


saveData : Bytes -> Cmd msg
saveData bytes =
    Download.bytes ("test.png") "image/png" bytes
