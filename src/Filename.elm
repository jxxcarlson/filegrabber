module Filename exposing (fromUrl, extension, mimeType)

import Parser exposing (..)
import Dict exposing (Dict)


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


pathFromUrl : String -> Maybe String
pathFromUrl url =
    case run parse url of
        Ok filename ->
            Just filename

        Err _ ->
            Nothing


fromUrl : String -> Maybe String
fromUrl url =
    pathFromUrl url
        |> Maybe.map (String.split "/")
        |> Maybe.map List.reverse
        |> Maybe.andThen List.head


{-| Filename.extension "<http://foo.a.jpg"> == Just "jpg" : Maybe String
Filename.extension "<http://foo"> == Nothing
-}
extension : String -> Maybe String
extension str =
    str
        |> fromUrl
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
