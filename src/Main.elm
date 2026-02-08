module Main exposing (main)

import Array
import Browser
import Element exposing (Element, px, rgba255)
import Element.Background
import Element.Border
import Element.Events as Events
import Element.Font
import Element.Input


type alias Word =
    { id : Int
    , word : String
    }


type alias Model =
    { words : List Word
    , input : String
    , canShowError : Bool
    , dragging : Maybe Int
    , over : Maybe Int
    }


init : Model
init =
    { words = emptyWords
    , input = ""
    , canShowError = False
    , dragging = Nothing
    , over = Nothing
    }


emptyWords : List Word
emptyWords =
    List.range 0 15
        |> List.map (\i -> { id = i, word = "" })


inputToWords : String -> List Word
inputToWords input =
    let
        wordsInInput : List Word
        wordsInInput =
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
            16 - List.length wordsInInput

        withEmptyWords words =
            words
                ++ (List.range (List.length wordsInInput) 15
                        |> List.map (\i -> { id = i, word = "" })
                   )
    in
    if missingCount == 0 then
        wordsInInput

    else
        wordsInInput |> withEmptyWords


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


findIndex : Int -> List Word -> Maybe Int
findIndex targetId words =
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
    step 0 words


swapAt : Int -> Int -> List Word -> List Word
swapAt i j words =
    if i == j then
        words

    else
        let
            arr =
                Array.fromList words

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


swapById : Int -> Int -> List Word -> List Word
swapById aId bId words =
    case ( findIndex aId words, findIndex bId words ) of
        ( Just i, Just j ) ->
            swapAt i j words

        _ ->
            words


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


viewWordTile : Model -> Int -> Word -> Element Msg
viewWordTile model index word =
    let
        isDragging =
            model.dragging == Just word.id

        isOver =
            model.over == Just word.id

        row : Int
        row =
            index // 4

        backgroundColor =
            case row of
                0 ->
                    rgba255 249 223 109 1

                1 ->
                    rgba255 160 195 90 1

                2 ->
                    rgba255 176 196 239 1

                _ ->
                    rgba255 186 129 197 1

        baseAttrs =
            [ Element.width (px 150)
            , Element.height (px 80)
            , Element.Background.color backgroundColor
            , Element.Border.rounded 6
            , Element.Font.center
            , Events.onMouseDown (DragStart word.id)
            , Events.onMouseEnter (DragEnter word.id)
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
        (Element.el
            [ Element.centerX
            , Element.centerY
            ]
            (Element.text word.word)
        )


view : Model -> Element Msg
view model =
    Element.column [ Element.width Element.fill, Element.height Element.fill ]
        [ Element.column [ Element.centerY, Element.centerX, Element.spacing 24 ]
            [ Element.el [ Element.width Element.fill, Element.Font.center ] (Element.text "Connections Companion")
            , Element.wrappedRow
                [ Element.width (px 624)
                , Element.spacing 8
                , Events.onMouseUp DragEnd
                ]
                (model.words |> List.indexedMap (\i w -> viewWordTile model i w))
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
