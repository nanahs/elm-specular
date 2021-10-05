module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import Math
import Platform.Sub as Sub
import Svg exposing (Svg)
import Svg.Attributes as SvgAttributes


type alias Model =
    { angle : Float
    , boxHeight : Int
    , boxWidth : Int
    , boxY : Int
    , eyePos : ( Int, Int )
    , peepWidth : Int
    , objPos : ( Int, Int )
    }


boxMin : Int
boxMin =
    50


boxMax : Int
boxMax =
    500


init : ( Model, Cmd Msg )
init =
    ( { angle = 0.0
      , boxHeight = boxMax
      , boxWidth = boxMax
      , boxY = 100
      , eyePos = ( 250, 50 )
      , peepWidth = 50
      , objPos = ( 250, 400 )
      }
    , Cmd.none
    )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Reflections"
    , body = [ viewContent model ]
    }


viewContent : Model -> Html Msg
viewContent model =
    let
        lineLength =
            (toFloat model.boxWidth / 2) / cos (degrees model.angle)
    in
    Html.div [ Attributes.class "antialiased text-center mt-14 mx-auto text-blue-900" ]
        [ Html.h1 [ Attributes.class "text-4xl font-bold text-blue-500" ] [ Html.text "Reflections" ]
        , Html.div [ Attributes.class "flex items-center justify-center space-x-20" ]
            [ Html.div [ Attributes.class "flex justify-center items-center mt-8" ]
                [ Svg.svg
                    [ SvgAttributes.viewBox <|
                        String.join " " <|
                            List.map String.fromInt <|
                                [ -100, 0, boxMax + 200, boxMax + 300 ]
                    , SvgAttributes.height "100%"
                    , SvgAttributes.width "100%"
                    , SvgAttributes.class "border border-red-500"
                    ]
                    [ Svg.node "svg"
                        [ SvgAttributes.y (String.fromInt model.boxY) ]
                        [ viewBoxEdges
                            { height = model.boxHeight
                            , width = model.boxWidth
                            , peep = model.peepWidth
                            }
                        , viewObj model.objPos
                        ]
                    , viewEye model.eyePos
                    , viewAngle model
                    ]
                ]
            , viewControls model
            ]
        ]


viewBoxEdges : { height : Int, width : Int, peep : Int } -> Svg msg
viewBoxEdges { height, width, peep } =
    let
        centerX =
            width // 2
    in
    Svg.g []
        [ Svg.rect
            [ SvgAttributes.x (String.fromInt ((boxMax // 2) - centerX))
            , SvgAttributes.y "0"
            , SvgAttributes.height (String.fromInt height)
            , SvgAttributes.width (String.fromInt width)
            , SvgAttributes.fill "white"
            , SvgAttributes.stroke "black"
            ]
            []
        , Svg.line
            [ SvgAttributes.stroke "blue"
            , SvgAttributes.x1 (String.fromInt ((boxMax // 2) - peep // 2))
            , SvgAttributes.y1 "0"
            , SvgAttributes.x2 (String.fromInt ((boxMax // 2) + peep // 2))
            , SvgAttributes.y2 "0"
            ]
            []
        ]


viewEye : ( Int, Int ) -> Svg msg
viewEye eyePos =
    Svg.circle
        [ SvgAttributes.cx ((String.fromInt << Tuple.first) eyePos)
        , SvgAttributes.cy ((String.fromInt << Tuple.second) eyePos)
        , SvgAttributes.r "10"
        ]
        []


viewObj : ( Int, Int ) -> Svg msg
viewObj eyePos =
    Svg.circle
        [ SvgAttributes.cx ((String.fromInt << Tuple.first) eyePos)
        , SvgAttributes.cy ((String.fromInt << Tuple.second) eyePos)
        , SvgAttributes.r "10"
        , SvgAttributes.fill "pink"
        ]
        []


viewAngle : Model -> Svg msg
viewAngle model =
    let
        length =
            100

        ( eyeX, eyeY ) =
            Tuple.mapBoth toFloat toFloat model.eyePos

        sideLength =
            toFloat model.boxWidth / 2

        hypLength =
            sideLength / cos (degrees model.angle)

        side2Length =
            sqrt ((hypLength ^ 2) - (sideLength ^ 2))

        angleX =
            if model.angle < 90.0 then
                eyeX + sideLength

            else
                eyeX - sideLength

        angleY =
            eyeY + side2Length

        lines =
            { x1 = eyeX
            , y1 = eyeY
            , x2 = angleX
            , y2 = angleY
            , x3 = (toFloat boxMax / 2) + sideLength
            , y3 = toFloat model.boxY
            , x4 = (toFloat boxMax / 2) + sideLength
            , y4 = toFloat model.boxY + side2Length
            }

        hasIntersection =
            Math.hasIntersection lines
    in
    Svg.line
        [ SvgAttributes.x1 (String.fromFloat eyeX)
        , SvgAttributes.y1 (String.fromFloat eyeY)
        , SvgAttributes.x2 (String.fromFloat angleX)
        , SvgAttributes.y2 (String.fromFloat angleY)
        , SvgAttributes.stroke <|
            if hasIntersection then
                "green"

            else
                "red"
        , SvgAttributes.strokeWidth "4"
        ]
        []


viewControls : Model -> Html Msg
viewControls model =
    Html.div [ Attributes.class "bg-grey-200", Attributes.style "width" "300px" ]
        [ sliderControl
            { label = "Angle"
            , value = model.angle
            , min = 0
            , max = 180
            , step = 0.5
            , onInput = ChangedAngle
            , decoder = Decode.floatDecoder
            }
        , sliderControl
            { label = "Box Width"
            , value = toFloat model.boxWidth
            , min = boxMin
            , max = boxMax
            , step = 1.0
            , onInput = ChangedBoxWidth
            , decoder = Decode.intDecoder
            }
        , sliderControl
            { label = "Box Height"
            , value = toFloat model.boxHeight
            , min = boxMin
            , max = boxMax
            , step = 1.0
            , onInput = ChangedBoxHeight
            , decoder = Decode.intDecoder
            }

        -- , sliderControl
        --     { label = "Peep Width"
        --     , value = toFloat model.peepWidth
        --     , min = 0
        --     , max = boxMax
        --     , step = 1
        --     , onInput = ChangedPeepWidth
        --     , decoder = Decode.intDecoder
        --     }
        ]


sliderControl :
    { label : String
    , value : Float
    , min : Int
    , max : Int
    , step : Float
    , onInput : number -> Msg
    , decoder : Decoder number
    }
    -> Html Msg
sliderControl config =
    let
        minStr =
            String.fromInt config.min

        maxStr =
            String.fromInt config.max
    in
    Html.label [ Attributes.class "flex space-x-2" ]
        [ Html.span [] [ Html.text (config.label ++ ": ") ]
        , Html.input
            [ onInput config.onInput config.decoder
            , Attributes.type_ "range"
            , Attributes.min minStr
            , Attributes.max maxStr
            , Attributes.value (String.fromFloat config.value)
            , Attributes.step (String.fromFloat config.step)
            ]
            []
        , Html.span [] [ Html.text (String.fromFloat config.value) ]
        ]


onInput : (number -> msg) -> Decoder number -> Html.Attribute msg
onInput tagger decoder =
    Events.on "input" (Decode.map tagger (Decode.at [ "target", "value" ] decoder))



-- UPDATE


type Msg
    = ChangedAngle Float
    | ChangedBoxHeight Int
    | ChangedBoxWidth Int
    | ChangedPeepWidth Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedAngle val ->
            ( { model | angle = val }, Cmd.none )

        ChangedBoxHeight val ->
            ( { model | boxHeight = val }, Cmd.none )

        ChangedBoxWidth val ->
            ( { model
                | boxWidth = val
                , peepWidth =
                    if model.peepWidth < val then
                        model.peepWidth

                    else
                        val
              }
            , Cmd.none
            )

        ChangedPeepWidth val ->
            ( { model | peepWidth = val }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Decode.Value Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
