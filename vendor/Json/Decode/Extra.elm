module Json.Decode.Extra exposing (floatDecoder, intDecoder)

import Json.Decode as Decode exposing (Decoder)


intDecoder : Decoder Int
intDecoder =
    Decode.oneOf
        [ Decode.int
        , Decode.string
            |> Decode.andThen
                (\s ->
                    String.toInt s
                        |> Maybe.map Decode.succeed
                        |> Maybe.withDefault (Decode.fail <| "Failed to convert " ++ s ++ " to int")
                )
        ]


floatDecoder : Decoder Float
floatDecoder =
    Decode.oneOf
        [ Decode.float
        , Decode.string
            |> Decode.andThen
                (\s ->
                    String.toFloat s
                        |> Maybe.map Decode.succeed
                        |> Maybe.withDefault (Decode.fail <| "Failed to convert " ++ s ++ " to float")
                )
        ]
