module Main exposing (main)

import Browser
import Html as H exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List.Extra as List
import Time



-- Model


type CallDir
    = Up
    | Down


type alias Call =
    { floor : Int
    , direction : CallDir
    }


type alias Elevator =
    { floor : Int
    , call : Maybe Call
    }


type alias Model =
    { es : List Elevator
    , calls : List Call
    }


initialModel : Model
initialModel =
    { es = List.repeat 5 { floor = 1, call = Nothing }
    , calls = []
    }



-- UPDATE


type Msg
    = AddCall Call
    | Tick


dispatchCalls : Model -> Model
dispatchCalls model =
    let
        { es, calls } =
            model
    in
    case calls of
        [] ->
            model

        c :: cs ->
            let
                isAlreadyOnTheWay =
                    List.member (Just c) (List.map (\e -> e.call) model.es)

                freeElevatorIndex =
                    List.findIndex (\e -> e.call == Nothing) model.es

                dispatchCall e =
                    { e
                        | call =
                            if c.floor == e.floor then
                                Nothing

                            else
                                Just c
                    }
            in
            if isAlreadyOnTheWay then
                model

            else
                case freeElevatorIndex of
                    Just index ->
                        { model | calls = cs, es = List.updateAt index dispatchCall model.es }

                    Nothing ->
                        model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddCall call ->
            ( dispatchCalls { model | calls = List.concat [ model.calls, [ call ] ] }, Cmd.none )

        Tick ->
            ( dispatchCalls { model | es = List.map tickE model.es }, Cmd.none )


tickE : Elevator -> Elevator
tickE e =
    let
        newFloor =
            case e.call of
                Just { floor } ->
                    if e.floor < floor then
                        e.floor + 1

                    else
                        e.floor - 1

                Nothing ->
                    e.floor

        stopIfReachedDest { floor } =
            if floor == newFloor then
                Nothing

            else
                e.call
    in
    { e
        | floor = newFloor
        , call = Maybe.andThen stopIfReachedDest e.call
    }



-- VIEW


view : Model -> Html Msg
view model =
    let
        viewElevator : Int -> Elevator -> Html Msg
        viewElevator i e =
            H.td []
                [ text <|
                    if i == e.floor then
                        "O"

                    else
                        ""
                ]

        viewFloor : List Elevator -> Int -> Html Msg
        viewFloor es i =
            H.tr [] <|
                List.concat
                    [ [ H.td [] [ text (String.fromInt i) ]
                      , H.td []
                            [ button [ onClick (AddCall { direction = Up, floor = i }) ] [ text "^" ]
                            , button [ onClick (AddCall { direction = Down, floor = i }) ] [ text "v" ]
                            ]
                      ]
                    , List.map (viewElevator i) es
                    ]
    in
    H.table [] <| List.map (viewFloor model.es) (List.reverse (List.range 1 10))


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , subscriptions = \model -> Time.every 500 (\_ -> Tick)
        , view = view
        , update = update
        }
