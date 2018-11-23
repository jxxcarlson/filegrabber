module Main exposing (main)

import Browser
import Html exposing (Html, button, input, div, text, pre, h1)
import Html.Events exposing (onClick, onInput)
import Bytes exposing (..)
import Bytes.Encode
import Http
import Html.Attributes exposing (style, value, placeholder)
import FileGrabber
import File.Download as Download
import Filename
import Tar exposing (Data(..), FileRecord, defaultFileRecord)


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
    , urlList : List String
    , dataList : List ( FileRecord, Data )
    }


initialModel : Model
initialModel =
    { status = "Starting up"
    , urlList = [ imageUrl1, imageUrl2 ]
    , dataList = []
    }


imageUrl =
    "https://natgeo.imgix.net/factsheets/thumbnails/01-frog-day-gallery.adapt.1900.1.jpg?auto=compress,format&w=1024&h=560&fit=crop"


imageUrl1 =
    "https://natgeo.imgix.net/factsheets/thumbnails/01-frog-day-gallery.adapt.1900.1.jpg?auto=compress,format&w=1024&h=560&fit=crop"


imageUrl2 =
    "https://le-www-live-s.legocdn.com/sc/media/lessons/wedo-2/wedo-projects/images/frogs-metamorphosis-project-image-feb9db40c70bcda57e12f5671d4bc278.jpg?fit=around|700:700&crop=700:700;*,*"


urlList =
    [ imageUrl1, imageUrl2 ]


type Msg
    = GetData
    | GotData String (Result Http.Error Bytes)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetData ->
            let
                maybeUrl =
                    List.head model.urlList

                nextUrlList =
                    List.drop 1 model.urlList
            in
                case maybeUrl of
                    Nothing ->
                        ( model, Cmd.none )

                    Just url ->
                        ( { model | urlList = nextUrlList }, getData url )

        GotData url result ->
            case result of
                Ok data ->
                    let
                        filename =
                            Filename.fromUrl url |> Maybe.withDefault "---"

                        fileRecord =
                            { defaultFileRecord | filename = filename }

                        newData =
                            ( fileRecord, BinaryData data )

                        newDataList =
                            newData :: model.dataList

                        nextUrlList =
                            List.drop 1 model.urlList

                        newModel =
                            { model
                                | status =
                                    --"Bytes received for " ++ String.left 10 filename ++ " = " ++ (String.fromInt (Bytes.width data))
                                    "Data items: " ++ String.fromInt (List.length newDataList)
                                , dataList = newDataList
                                , urlList = nextUrlList
                            }

                        cmd =
                            case List.head model.urlList of
                                Nothing ->
                                    let
                                        bytes =
                                            Tar.encodeFiles newDataList |> Bytes.Encode.encode
                                    in
                                        Download.bytes ("test.tar") "application/x-tar" bytes

                                Just url_ ->
                                    getData url_
                    in
                        ( newModel, cmd )

                Err _ ->
                    ( { model | status = "Invalid data" }, Cmd.none )


view : Model -> Html Msg
view model =
    div outerStyle
        [ h1 [ style "font-size" "20px", style "margin-bottom" "20px" ] [ text "Image grabber" ]
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
