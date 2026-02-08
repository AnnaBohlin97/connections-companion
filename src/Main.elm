module Main exposing (main)

import Array
import Browser
import Element exposing (Element, px, rgba255)
import Element.Background
import Element.Border
import Element.Events as Events
import Element.Font
import Element.Input


type alias Tile =
    { id : Int
    , word : String
    }


type alias Model =
    { words : List Tile
    , input : String
    , canShowError : Bool
    , dragging : Maybe Int
    , over : Maybe Int
    }


init : Model
init =
    { words = emptyTiles
    , input = ""
    , canShowError = False
    , dragging = Nothing
    , over = Nothing
    }


emptyTiles : List Tile
emptyTiles =
    List.range 0 15
        |> List.map (\i -> { id = i, word = "" })


inputToWords : String -> List Tile
inputToWords input =
    let
        provided : List Tile
        provided =
            input
                |> String.split ","
                |> List.take 16
                |> List.indexedMap
                    (\index word ->
                        { id = index
                        , word = String.trim word
                        }
                    )

        missingCount : Int
        missingCount =
            16 - List.length provided

        padded : List Tile
        padded =
            if missingCount <= 0 then
                provided

            else
                provided
                    ++ (List.range (List.length provided) 15
                            |> List.map (\i -> { id = i, word = "" })
                       )
    in
    padded


type Msg
    = InputChanged String
    | DragStart Int
    | DragEnter Int
    | DragEnd


validateInput : String -> Bool
validateInput input =
    let
        wordList =
            String.split "," input

        hasDuplicates list =
            case list of
                [] ->
                    False

                x :: xs ->
                    List.member x xs || hasDuplicates xs
    in
    List.length wordList <= 16 && not (hasDuplicates wordList)


findIndex : Int -> List Tile -> Maybe Int
findIndex targetId tiles =
    let
        step index remaining =
            case remaining of
                [] ->
                    Nothing

                head :: tail ->
                    if head.id == targetId then
                        Just index

                    else
                        step (index + 1) tail
    in
    step 0 tiles


swapAt : Int -> Int -> List Tile -> List Tile
swapAt i j tiles =
    if i == j then
        tiles

    else
        let
            arr =
                Array.fromList tiles

            a =
                Array.get i arr

            b =
                Array.get j arr

            swapped =
                case ( a, b ) of
                    ( Just ta, Just tb ) ->
                        arr
                            |> Array.set i tb
                            |> Array.set j ta

                    _ ->
                        arr
        in
        Array.toList swapped


swapById : Int -> Int -> List Tile -> List Tile
swapById aId bId tiles =
    case ( findIndex aId tiles, findIndex bId tiles ) of
        ( Just i, Just j ) ->
            swapAt i j tiles

        _ ->
            tiles


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputChanged newText ->
            { model
                | input = newText
                , words = inputToWords newText
                , canShowError =
                    if validateInput newText then
                        False

                    else
                        True
            }

        DragStart tileId ->
            { model | dragging = Just tileId, over = Just tileId }

        DragEnter tileId ->
            case model.dragging of
                Nothing ->
                    model

                Just _ ->
                    { model | over = Just tileId }

        DragEnd ->
            case ( model.dragging, model.over ) of
                ( Just aId, Just bId ) ->
                    { model
                        | words = swapById aId bId model.words
                        , dragging = Nothing
                        , over = Nothing
                    }

                _ ->
                    { model | dragging = Nothing, over = Nothing }


viewTile : Model -> Tile -> Element Msg
viewTile model tile =
    let
        isDragging =
            model.dragging == Just tile.id

        isOver =
            model.over == Just tile.id

        baseAttrs =
            [ Element.width (px 200)
            , Element.Background.color (rgba255 245 126 215 1)
            , Element.padding 30
            , Element.Border.rounded 12
            , Element.Font.center
            , Events.onMouseDown (DragStart tile.id)
            , Events.onMouseEnter (DragEnter tile.id)
            , Events.onMouseUp DragEnd
            ]

        dragAttrs =
            if isDragging then
                [ Element.alpha 0.6 ]

            else
                []

        overAttrs =
            if not isDragging && isOver then
                [ Element.Border.width 2 ]

            else
                []
    in
    Element.el (baseAttrs ++ dragAttrs ++ overAttrs)
        (Element.text tile.word)


view : Model -> Element Msg
view model =
    Element.column [ Element.width Element.fill, Element.height Element.fill ]
        [ Element.column [ Element.centerY, Element.centerX, Element.spacing 24 ]
            [ Element.el [ Element.width Element.fill, Element.Font.center ] (Element.text "Connections Companion")
            , Element.wrappedRow
                [ Element.width (px 824)
                , Element.spacing 8
                , Events.onMouseUp DragEnd
                ]
                (model.words |> List.map (viewTile model))
            , Element.Input.text
                [ Element.Border.rounded 12 ]
                { onChange = InputChanged, text = model.input, placeholder = Nothing, label = Element.Input.labelHidden "" }
            , if model.canShowError then
                Element.text "Please enter no more than 16 unique words, separated by commas."

              else
                Element.none
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view =
            \model ->
                Element.layout []
                    (view model)
        }



-- hej, jag, heter, anna, och, det, här, är, alla, ord, som, ska, vara, med, i, listan
