module ImageGrabber
    exposing
        ( expectBytes
        , getImageTask
        , filenameFromUrl
        , extension
        , mimeType
        , saveBytes
        , downloadTarArchiveCmd
        )

import Bytes exposing (Bytes)
import Bytes.Decode exposing (Decoder)
import Bytes.Encode exposing (encode)
import File.Download as Download
import Http
import Parser exposing (..)
import Dict exposing (Dict)
import Task exposing (Task)
import Tar exposing (Data(..), FileRecord, defaultFileRecord)


downloadTarArchiveCmd : List ( String, Bytes ) -> Cmd msg
downloadTarArchiveCmd dataList =
    let
        archive =
            Tar.encodeFiles (List.map prepareData dataList) |> encode
    in
        saveBytes "archive" archive


saveBytes : String -> Bytes -> Cmd msg
saveBytes archiveName bytes =
    Download.bytes (archiveName ++ ".tar") "application/x-tar" bytes


prepareData : ( String, Bytes ) -> ( FileRecord, Data )
prepareData ( url, bytes ) =
    case filenameFromUrl url of
        Nothing ->
            ( defaultFileRecord, BinaryData bytes )

        Just filename ->
            ( { defaultFileRecord | filename = filename }, BinaryData bytes )


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


expectBytes : (Result Http.Error Bytes -> msg) -> Http.Expect msg
expectBytes toMsg =
    Http.expectBytesResponse toMsg <|
        \response ->
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



--
-- FIlENAME PARSER
--


parse : Parser String
parse =
    succeed identity
        |. oneOf [ symbol "http://", symbol "https://" ]
        |= parseBody


parseBody : Parser String
parseBody =
    getChompedString <|
        succeed ()
            |. chompWhile (\c -> c /= '?')


filenameFromUrl : String -> Maybe String
filenameFromUrl url =
    case run parse url of
        Ok filename ->
            Just filename

        Err _ ->
            Nothing


{-| Filename.extension "<http://foo.a.jpg"> == Just "jpg" : Maybe String
Filename.extension "<http://foo"> == Nothing
-}
extension : String -> Maybe String
extension str =
    str
        |> filenameFromUrl
        |> Maybe.map (String.split ".")
        |> Maybe.map List.reverse
        |> Maybe.andThen filter
        |> Maybe.andThen List.head


filter : List String -> Maybe (List String)
filter list =
    if List.length list < 2 then
        Nothing
    else
        Just list


mimeTypeDict =
    Dict.fromList
        [ ( "png", "image/png" )
        , ( "jpg", "image/jpeg" )
        , ( "jpeg", "image/jpeg" )
        , ( "gif", "image/gif" )
        , ( "svg", "image/svg+xml" )
        ]


{-| Filename.mimeType "<http://foo.a.jpg"> == Just "image/jpeg"
Filename.mimeType "<http://foo.a.yak"> ==nNothing
-}
mimeType : String -> Maybe String
mimeType url =
    url
        |> extension
        |> Maybe.andThen (\x -> Dict.get x mimeTypeDict)
