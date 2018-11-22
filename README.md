# FileGrabber

__FileGrabber__:  User pure Elm to grab a file from  a URL, transform the received data into a `Bytes` value, display the `Bytes.width` of that value, and download the data to disk using `File.Download`.  All this currently works with the common image types.  You can expand the scope of the app by adding to the `mimeTypeDict` (below).  The app needs to know the mimetype of the data to download it.

```
module Filename ...

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
