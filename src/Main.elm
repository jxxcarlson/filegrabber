module Main exposing (main)

import Browser
import Html exposing (Html, button, input, div, text, pre, h1)
import Html.Events exposing (onClick, onInput)
import Bytes exposing (..)
import Http
import Html.Attributes exposing (style, value, placeholder)
import FileGrabber
import File.Download as Download
import Filename


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
    , url : String
    , maybeBytes : Maybe Bytes
    }


initialModel : Model
initialModel =
    { status = "Starting up"
    , url = imageUrl
    , maybeBytes = Nothing
    }


imageUrl =
    "https://natgeo.imgix.net/factsheets/thumbnails/01-frog-day-gallery.adapt.1900.1.jpg?auto=compress,format&w=1024&h=560&fit=crop"


type Msg
    = AcceptUrl String
    | GetData
    | GotData String (Result Http.Error Bytes)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AcceptUrl str ->
            ( { model | url = str }, Cmd.none )

        GetData ->
            ( model, getData model.url )

        GotData url result ->
            case result of
                Ok data ->
                    let
                        filename =
                            Filename.fromUrl url |> Maybe.withDefault "---"

                        newModel =
                            { model
                                | status =
                                    "Bytes received for " ++ String.left 10 filename ++ " = " ++ (String.fromInt (Bytes.width data))
                                , maybeBytes = Just data
                            }
                    in
                        ( newModel, saveData newModel )

                Err _ ->
                    ( { model | status = "Invalid data" }, Cmd.none )


view : Model -> Html Msg
view model =
    div outerStyle
        [ h1 [ style "font-size" "20px", style "margin-bottom" "20px" ] [ text "Image grabber" ]
        , input (inputAttributes model) []
        , button buttonAttributes [ text "Get image" ]
        , pre [] [ text <| "status: " ++ model.status ]
        ]


outerStyle =
    [ style "margin" "40px"
    , style "padding" "20px"
    , style "background-color" "#eee"
    , style "width" "600px"
    , style "font-size" "14px"
    ]


inputAttributes model =
    [ placeholder "Image url"
    , value model.url
    , onInput AcceptUrl
    , style "width" "580px"
    , style "display" "block"
    , style "font-size" "14px"
    , style "margin-bottom" "20px"
    ]


buttonAttributes =
    [ onClick GetData
    , style "background-color" "#444"
    , style "color" "#eee"
    , style "height" "30px"
    , style "font-size" "14px"
    , style "margin-bottom" "12px"
    ]


getData : String -> Cmd Msg
getData url =
    Http.get
        { url = url
        , expect = FileGrabber.expectBytes (GotData url)
        }


saveData : Model -> Cmd msg
saveData model =
    let
        maybeFilename =
            Filename.fromUrl model.url

        maybeMimeType =
            Filename.mimeType model.url
    in
        case ( maybeFilename, maybeMimeType, model.maybeBytes ) of
            ( Just filename, Just mimeType, Just bytes ) ->
                Download.bytes filename mimeType bytes

            ( _, _, _ ) ->
                Cmd.none
