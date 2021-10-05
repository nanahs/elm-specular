module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Math
import Test exposing (..)


suite : Test
suite =
    test "two plus two equals four"
        (\_ -> Expect.equal 4 (2 + 2))


intersectionTests : Test
intersectionTests =
    describe "Intersect"
        [ test "segments intersect at (0, 0)" <|
            \_ ->
                Expect.equal ( 0.0, 0.0 ) <|
                    Math.intersection
                        { x1 = -1.0
                        , y1 = 0.0
                        , x2 = 1.0
                        , y2 = 0.0
                        , x3 = 0.0
                        , y3 = -1.0
                        , x4 = 0.0
                        , y4 = 1.0
                        }
        , test "segments are parallel" <|
            \_ ->
                let
                    ( x, y ) =
                        Tuple.mapBoth isInfinite isInfinite <|
                            Math.intersection
                                { x1 = -1.0
                                , y1 = -1.0
                                , x2 = 1.0
                                , y2 = 1.0
                                , x3 = 0.0
                                , y3 = 0.0
                                , x4 = 2.0
                                , y4 = 2.0
                                }
                in
                Expect.equal True (x || y)
        , test "segments do not intersect and are not parallel" <|
            \_ ->
                Expect.equal ( 0.0, 0.0 ) <|
                    Math.intersection
                        { x1 = 0.0
                        , y1 = 0.0
                        , x2 = 2.0
                        , y2 = 0.0
                        , x3 = 5.0
                        , y3 = 0.0
                        , x4 = 5.0
                        , y4 = 5.0
                        }
        ]
