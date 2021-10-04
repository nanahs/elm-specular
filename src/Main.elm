module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events exposing (..)


type Msg
    = Increment
    | Decrement


update : Msg -> number -> number
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


view : Int -> Html Msg
view model =
    Html.div [ Attributes.class "antialiased text-center mt-14 mx-auto text-blue-900" ]
        [ Html.h1 [ Attributes.class "text-4xl font-bold text-blue-500" ] [ Html.text "With TailwindCSS" ]
        , Html.img
            [ Attributes.src "/logo.png"
            , Attributes.class "mx-auto w-80 mt-14"
            ]
            []
        , helloWorld model
        ]


helloWorld : Int -> Html Msg
helloWorld model =
    Html.div []
        [ Html.h1 [] [ Html.text "Hello, Vite + Elm!" ]
        , Html.p []
            [ Html.a
                [ Attributes.href "https://vitejs.dev/guide/features.html"
                , Attributes.class "text-blue-300"
                ]
                [ Html.text "Vite Documentation" ]
            , Html.text " | "
            , Html.a
                [ Attributes.href "https://guide.elm-lang.org/"
                , Attributes.class "text-blue-300"
                ]
                [ Html.text "Elm Documentation" ]
            ]
        , Html.button [ onClick Increment ] [ Html.text ("count is: " ++ String.fromInt model) ]
        , Html.p []
            [ Html.text "Edit "
            , Html.code [] [ Html.text "src/Main.elm" ]
            , Html.text " to test auto refresh"
            ]
        ]


main : Program () Int Msg
main =
    Browser.sandbox { init = 0, update = update, view = view }
