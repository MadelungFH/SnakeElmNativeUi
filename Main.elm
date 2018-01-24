module Main exposing (..)

import NativeUi as Ui exposing (Node)
import NativeUi.Elements as Elements exposing (..)
import NativeUi.Events exposing (..)
import NativeUi.Image as Image exposing (..)
import NativeUi.Style as Style exposing (defaultTransform)
import Random
import Time exposing (Time, millisecond)


-- Types


type alias Pos =
    ( Int, Int )


type Msg
    = NewFruit Pos
    | Tick Time
    | NoOp
    | ChangeUp
    | ChangeDown
    | ChangeLeft
    | ChangeRight


type alias Model =
    { field : List Pos
    , apple : Pos
    , snake : List Pos
    , dir : Direction
    }


type Direction
    = Up
    | Down
    | Left
    | Right



-- Konstanten


columns =
    30


rows =
    15


initField : List Pos
initField =
    [ ( 1, 1 ) ]


colorField : String
colorField =
    "#DAC27C"


colorSnake : String
colorSnake =
    "#59982F"


colorApple : String
colorApple =
    "#FF0800"



-- Initialisieren


initialGame : Model
initialGame =
    { field = createList initField
    , apple = ( 0, 0 )
    , snake = [ ( 1, 1 ) ]
    , dir = Left
    }


createModel : ( Model, Cmd Msg )
createModel =
    let
        model =
            initialGame
    in
    ( model, Random.generate NewFruit randPos )


createList : List Pos -> List Pos
createList list =
    case list of
        [] ->
            [ ( 1, 1 ) ]

        ( i, j ) :: x ->
            if j < columns then
                createList (( i, j + 1 ) :: list)
            else if i < rows then
                createList (( i + 1, 1 ) :: list)
            else
                list



-- Funktionen


randPos : Random.Generator Pos
randPos =
    Random.pair (Random.int 1 rows) (Random.int 1 columns)


collideWalls : List Pos -> Bool
collideWalls snake =
    case snake of
        ( i, j ) :: y ->
            i < 0 || i > rows || j < 0 || j > columns

        [] ->
            False


collideSnake : List Pos -> Bool
collideSnake snakeList =
    let
        snakeHead =
            List.head snakeList

        snakeBody =
            List.tail snakeList
    in
    case snakeBody of
        Nothing ->
            False

        Just x ->
            case snakeHead of
                Nothing ->
                    False

                Just y ->
                    collideSnakeHelper y x


collideSnakeHelper : Pos -> List Pos -> Bool
collideSnakeHelper snakeHead tailList =
    case tailList of
        [] ->
            False

        x :: y ->
            if x == snakeHead then
                True
            else
                collideSnakeHelper snakeHead y


eatApple : List Pos -> Pos -> Bool
eatApple snake apple =
    let
        snakeHead =
            List.head snake
    in
    case snakeHead of
        Just x ->
            x == apple

        Nothing ->
            False


stepSnake : List Pos -> Direction -> List Pos
stepSnake snake dir =
    case snake of
        [] ->
            [ ( 1, 1 ) ]

        ( i, j ) :: x ->
            case dir of
                Up ->
                    ( i + 1, j ) :: snake

                Down ->
                    ( i - 1, j ) :: snake

                Left ->
                    ( i, j + 1 ) :: snake

                Right ->
                    ( i, j - 1 ) :: snake


takeTail : List Pos -> List Pos
takeTail snake =
    List.drop 1 (List.reverse snake)


appleOnSnake : Pos -> List Pos -> Bool
appleOnSnake apple snake =
    case snake of
        [] ->
            False

        x :: y ->
            if x == apple then
                True
            else
                appleOnSnake apple y



-- View


view : Model -> Node Msg
view model =
    createGameField model


createGameField : Model -> Node Msg
createGameField model =
    Elements.view
        [ Ui.style
            [ Style.flex 1
            , Style.alignItems "center"
            , Style.justifyContent "space-between"
            ]
        ]
        [ Elements.view
            [ Ui.style
                [ Style.flexWrap "wrap"
                , Style.justifyContent "center"
                , Style.flexDirection "row"
                , Style.alignItems "center"
                , Style.width 600
                , Style.height 300
                ]
            ]
            (List.map (createGameBox model) model.field)
        , Elements.view
            [ Ui.style
                [ Style.width 50
                , Style.flexDirection "row"
                , Style.justifyContent "space-between"
                ]
            ]
            [ button ChangeUp "Up"
            ]
        , Elements.view
            [ Ui.style
                [ Style.width 200
                , Style.flexDirection "row"
                , Style.justifyContent "space-between"
                ]
            ]
            [ button ChangeLeft "Left"
            , button ChangeDown "Down"
            , button ChangeRight "Right"
            ]
        ]


createGameBox : Model -> Pos -> Node Msg
createGameBox model pos =
    if pos == model.apple then
        fieldBox colorApple
    else if isSnake pos model.snake then
        fieldBox colorSnake
    else
        fieldBox colorField


isSnake : Pos -> List Pos -> Bool
isSnake pos snake =
    case snake of
        [] ->
            False

        x :: y ->
            if x == pos then
                True
            else
                isSnake pos y


fieldBox : String -> Node Msg
fieldBox col =
    Elements.view
        [ Ui.style
            [ Style.width 20
            , Style.height 20
            , Style.backgroundColor col
            ]
        ]
        []


button : Msg -> String -> Node Msg
button msg content =
    text
        [ Ui.style
            [ Style.color "white"
            , Style.textAlign "center"
            , Style.backgroundColor "#000000"
            , Style.paddingTop 5
            , Style.paddingBottom 5
            , Style.width 50
            , Style.height 30
            , Style.fontWeight "bold"
            , Style.shadowColor "#000"
            , Style.shadowOpacity 0.25
            , Style.shadowOffset 1 1
            , Style.shadowRadius 5
            ]
        , onPress msg
        ]
        [ Ui.string content ]



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewFruit pos ->
            if appleOnSnake pos model.snake then
                ( model, Random.generate NewFruit randPos )
            else
                ( { model | apple = pos }, Cmd.none )

        Tick _ ->
            if collideWalls model.snake || collideSnake model.snake then
                createModel
            else
                let
                    snake_ =
                        stepSnake model.snake model.dir
                in
                if eatApple model.snake model.apple then
                    ( { model | snake = snake_ }, Random.generate NewFruit randPos )
                else
                    ( { model | snake = List.reverse (takeTail snake_) }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        ChangeUp ->
            if model.dir /= Down then
                ( { model | dir = Up }, Cmd.none )
            else
                ( model, Cmd.none )

        ChangeDown ->
            if model.dir /= Up then
                ( { model | dir = Down }, Cmd.none )
            else
                ( model, Cmd.none )

        ChangeLeft ->
            if model.dir /= Right then
                ( { model | dir = Left }, Cmd.none )
            else
                ( model, Cmd.none )

        ChangeRight ->
            if model.dir /= Left then
                ( { model | dir = Right }, Cmd.none )
            else
                ( model, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (500 * Time.millisecond) Tick
        ]



-- main


main : Program Never Model Msg
main =
    Ui.program
        { init = createModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }