module Main exposing (main)

import Browser
import Html exposing (Html, button, input, div, text, pre, h1)
import Html.Events exposing (onClick, onInput)
import Bytes exposing (..)
import Bytes.Encode exposing (encode)
import Http
import Html.Attributes exposing (style, value, placeholder)
import ImageGrabber
import File.Download as Download
import Task exposing (Task)
import Json.Encode as Encode
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
    , url : String
    , maybeBytes : Maybe Bytes
    , urlList : List String
    , dataList : List ( String, Bytes )
    }


initialModel : Model
initialModel =
    { status = "Starting up"
    , url = imageUrl
    , maybeBytes = Nothing
    , urlList = [ imageUrl2, imageUrl ]
    , dataList = []
    }


imageUrl =
    "https://natgeo.imgix.net/factsheets/thumbnails/01-frog-day-gallery.adapt.1900.1.jpg?auto=compress,format&w=1024&h=560&fit=crop"


imageUrl2 =
    "https://images.theconversation.com/files/117973/original/image-20160408-23649-1qxbogn.jpg?ixlib=rb-1.1.0&rect=0%2C516%2C2537%2C1652&q=45&auto=format&w=926&fit=clip"


type Msg
    = AcceptUrl String
    | GetData
    | GotData (Result Http.Error Bytes)


getImageTask : String -> Task Http.Error Bytes
getImageTask url_ =
    Http.task
        { method = "get"
        , headers = []
        , url = url_
        , body = Http.emptyBody
        , resolver = Http.bytesResolver bytesResponse
        , timeout = Nothing
        }


bytesResponse : Http.Response Bytes -> Result Http.Error Bytes
bytesResponse response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata body ->
            Err (Http.BadStatus metadata.statusCode)

        Http.GoodStatus_ metadata body ->
            Ok body


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AcceptUrl str ->
            ( { model | url = str }, Cmd.none )

        GetData ->
            case List.head model.urlList of
                Nothing ->
                    ( model, Cmd.none )

                Just url ->
                    ( { model | url = url }, getDataFromList model )

        GotData result ->
            case result of
                Ok data ->
                    case List.head model.urlList of
                        Nothing ->
                            ( model, Cmd.none )

                        Just url ->
                            let
                                newModel =
                                    { model
                                        | status =
                                            "Bytes received = " ++ (String.fromInt (Bytes.width data))
                                        , maybeBytes = Just data
                                        , urlList = List.drop 1 model.urlList
                                        , dataList = ( url, data ) :: model.dataList
                                    }
                            in
                                ( newModel, getDataFromList newModel )

                Err _ ->
                    ( { model | status = "Invalid data" }, Cmd.none )


downloadTarArchiveCmd : Model -> Cmd Msg
downloadTarArchiveCmd model =
    let
        archive =
            Tar.encodeFiles (List.map prepareData model.dataList) |> encode
    in
        saveBytes "archive" archive


prepareData : ( String, Bytes ) -> ( FileRecord, Data )
prepareData ( url, bytes ) =
    case ImageGrabber.filenameFromUrl url of
        Nothing ->
            ( defaultFileRecord, BinaryData bytes )

        Just filename ->
            ( { defaultFileRecord | filename = filename }, BinaryData bytes )


getDataFromList : Model -> Cmd Msg
getDataFromList model =
    case List.head model.urlList of
        Nothing ->
            downloadTarArchiveCmd model

        Just url ->
            getData url


getData : String -> Cmd Msg
getData url_ =
    Task.attempt GotData (getImageTask url_)


saveBytes : String -> Bytes -> Cmd msg
saveBytes archiveName bytes =
    Download.bytes (archiveName ++ ".tar") "application/x-tar" bytes



--
-- VIEW
--


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
