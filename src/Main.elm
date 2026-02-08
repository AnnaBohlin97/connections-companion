module Main exposing (main)

import Array
import Browser
import Browser.Dom as Dom
import Browser.Events
import Element exposing (Element, px, rgba255)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Attr
import Task


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
    , viewportWidth : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { words = emptyWords
      , input = ""
      , canShowError = False
      , dragging = Nothing
      , over = Nothing
      , viewportWidth = 1024
      }
    , Dom.getViewport
        |> Task.attempt GotViewport
    )


isMobile : Int -> Bool
isMobile viewportWidth =
    viewportWidth <= 440


emptyWords : List Word
emptyWords =
    List.range 0 15
        |> List.map (\i -> { id = i, word = "" })


type Msg
    = InputChanged String
    | DragStart Int
    | DragEnter Int
    | DragEnd
    | GotViewport (Result Dom.Error Dom.Viewport)
    | WindowResized Int Int


inputToWords : String -> List Word
inputToWords input =
    let
        wordsInInput : List Word
        wordsInInput =
            input
                |> String.toUpper
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
                ( Just idA, Just idB ) ->
                    { model
                        | words = swapById idA idB model.words
                        , dragging = Nothing
                        , over = Nothing
                    }

                _ ->
                    { model | dragging = Nothing, over = Nothing }

        GotViewport result ->
            case result of
                Ok viewport ->
                    { model | viewportWidth = round viewport.viewport.width }

                Err _ ->
                    model

        WindowResized width _ ->
            { model | viewportWidth = width }


type alias LayoutSizes =
    { tileWidth : Int
    , tileHeight : Int
    , gridWidth : Int
    }


layoutSizes : Model -> LayoutSizes
layoutSizes model =
    let
        spacing =
            8

        maxTileWidth =
            150

        maxTileHeight =
            150

        horizontalPadding =
            16

        usableWidth =
            max 0 (model.viewportWidth - (2 * horizontalPadding))

        -- 4 columns: 4 tiles + 3 gaps
        candidateTileWidth =
            (usableWidth - (3 * spacing)) // 4

        tileWidth =
            clamp 80 maxTileWidth candidateTileWidth

        tileHeight =
            if isMobile model.viewportWidth then
                -- Square tiles on mobile (1:1).
                tileWidth

            else
                -- Desktop ratio 80/150.
                clamp 44 maxTileHeight ((tileWidth * maxTileHeight) // maxTileWidth)

        gridWidth =
            (4 * tileWidth) + (3 * spacing)
    in
    { tileWidth = tileWidth, tileHeight = tileHeight, gridWidth = gridWidth }


viewWordTile : Model -> LayoutSizes -> Int -> Word -> Element Msg
viewWordTile model sizes index word =
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

        fontSize =
            let
                length =
                    String.length word.word
            in
            if isMobile model.viewportWidth then
                if length <= 5 then
                    20

                else if length == 6 then
                    18

                else if length == 7 then
                    16

                else if length == 8 then
                    14

                else if length == 9 then
                    12

                else
                    10

            else if length <= 8 then
                20

            else if length <= 9 then
                18

            else if length <= 10 then
                16

            else
                14

        baseAttrs =
            [ Element.width (px sizes.tileWidth)
            , Element.height (px sizes.tileHeight)
            , Element.htmlAttribute (Attr.style "user-select" "none")
            , Element.htmlAttribute (Attr.style "-webkit-user-select" "none")
            , Background.color backgroundColor
            , Border.rounded 6
            , Font.center
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
                [ Border.width 2 ]

            else
                []
    in
    Element.el (baseAttrs ++ dragAttrs ++ overAttrs)
        (Element.el
            [ Element.centerX
            , Element.centerY
            ]
            (Element.paragraph [ Font.size fontSize ] [ Element.text word.word ])
        )


view : Model -> Element Msg
view model =
    let
        sizes =
            layoutSizes model

        headerFontSize =
            if isMobile model.viewportWidth then
                20

            else
                32

        bodyFontSize =
            if isMobile model.viewportWidth then
                14

            else
                20
    in
    Element.column [ Element.width Element.fill, Element.height Element.fill ]
        [ Element.column [ Element.width (px sizes.gridWidth), Element.centerY, Element.centerX, Element.spacing 24 ]
            [ Element.el
                [ Element.width Element.fill, Font.center, Font.size headerFontSize, Font.bold ]
                (Element.text "Connections Companion:\nSort your words before submitting!")
            , Element.paragraph
                [ Font.center, Font.size bodyFontSize ]
                [ Element.text "Enter today's words, drag and drop the tiles to sort them, and feel confident submitting your Connections!" ]
            , Element.wrappedRow
                [ Element.spacing 8
                , Events.onMouseUp DragEnd
                , Element.htmlAttribute (Attr.style "user-select" "none")
                , Element.htmlAttribute (Attr.style "-webkit-user-select" "none")
                ]
                (model.words |> List.indexedMap (\i w -> viewWordTile model sizes i w))
            , Input.text
                [ Border.rounded 12 ]
                { onChange = InputChanged
                , text = model.input
                , placeholder = Just (Input.placeholder [ Font.size bodyFontSize ] (Element.text "Type today's words here, separated by commas"))
                , label = Input.labelHidden "Today's words"
                }
            , if model.canShowError then
                Element.text "Please enter no more than 16 unique words, separated by commas."

              else
                Element.none
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize WindowResized


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        , view =
            \model ->
                Element.layout
                    [ Font.family
                        [ Font.typeface "Libre Franklin"
                        , Font.sansSerif
                        ]
                    ]
                    (view model)
        }



-- hej, jag, heter, anna, och, det, här, är, alla, ord, som, ska, vara, med, i, listan
