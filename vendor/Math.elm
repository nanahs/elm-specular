module Math exposing (distance, hasIntersection, intersection)


distance : ( Float, Float ) -> ( Float, Float ) -> Float
distance ( x1, y1 ) ( x2, y2 ) =
    sqrt (((x2 - x1) ^ 2) + ((y2 - y1) ^ 2))


hasIntersection :
    { x1 : Float
    , y1 : Float
    , x2 : Float
    , y2 : Float
    , x3 : Float
    , y3 : Float
    , x4 : Float
    , y4 : Float
    }
    -> Bool
hasIntersection config =
    let
        ( x, y ) =
            intersection config
    in
    if isInfinite x then
        False

    else if isInfinite y then
        False

    else
        (max config.x1 config.x2 >= min config.x3 config.x4)
            && (max config.x3 config.x4 >= min config.x1 config.x2)
            && (max config.y1 config.y2 >= min config.y3 config.y4)
            && (max config.y3 config.y4 >= min config.y1 config.y2)


intersection :
    { x1 : Float
    , y1 : Float
    , x2 : Float
    , y2 : Float
    , x3 : Float
    , y3 : Float
    , x4 : Float
    , y4 : Float
    }
    -> ( Float, Float )
intersection { x1, y1, x2, y2, x3, y3, x4, y4 } =
    let
        d =
            ((x1 - x2) * (y3 - y4)) - ((y1 - y2) * (x3 - x4))

        x =
            (((x1 * y2) - (y1 * x2) * (x3 - x4))
                - (((x3 * y4) - (y3 * x4)) * (x1 - x2))
            )
                / d

        y =
            ((((x1 * y2) - (y1 * x2)) * (y3 - y4))
                - (((x3 * y4) - (y3 * x4)) * (y1 - y2))
            )
                / d
    in
    ( x, y )
