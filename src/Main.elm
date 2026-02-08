module Main exposing (main)

import Browser
import Element exposing (Element, px, rgba255)
import Element.Background
import Element.Border
import Element.Font
import Element.Input


type alias Model =
    { words : Words
    , input : String
    , canShowError : Bool
    }


type alias Words =
    { word1 : String
    , word2 : String
    , word3 : String
    , word4 : String
    , word5 : String
    , word6 : String
    , word7 : String
    , word8 : String
    , word9 : String
    , word10 : String
    , word11 : String
    , word12 : String
    , word13 : String
    , word14 : String
    , word15 : String
    , word16 : String
    }


init : Model
init =
    { words =
        { word1 = ""
        , word2 = ""
        , word3 = ""
        , word4 = ""
        , word5 = ""
        , word6 = ""
        , word7 = ""
        , word8 = ""
        , word9 = ""
        , word10 = ""
        , word11 = ""
        , word12 = ""
        , word13 = ""
        , word14 = ""
        , word15 = ""
        , word16 = ""
        }
    , input = ""
    , canShowError = False
    }


wordsToList : Words -> List String
wordsToList words =
    [ words.word1
    , words.word2
    , words.word3
    , words.word4
    , words.word5
    , words.word6
    , words.word7
    , words.word8
    , words.word9
    , words.word10
    , words.word11
    , words.word12
    , words.word13
    , words.word14
    , words.word15
    , words.word16
    ]


type Msg
    = InputChanged String


inputToWords : String -> Words
inputToWords input =
    let
        wordList =
            String.split "," input
    in
    { word1 = List.head wordList |> Maybe.withDefault ""
    , word2 = List.head (List.drop 1 wordList) |> Maybe.withDefault ""
    , word3 = List.head (List.drop 2 wordList) |> Maybe.withDefault ""
    , word4 = List.head (List.drop 3 wordList) |> Maybe.withDefault ""
    , word5 = List.head (List.drop 4 wordList) |> Maybe.withDefault ""
    , word6 = List.head (List.drop 5 wordList) |> Maybe.withDefault ""
    , word7 = List.head (List.drop 6 wordList) |> Maybe.withDefault ""
    , word8 = List.head (List.drop 7 wordList) |> Maybe.withDefault ""
    , word9 = List.head (List.drop 8 wordList) |> Maybe.withDefault ""
    , word10 = List.head (List.drop 9 wordList) |> Maybe.withDefault ""
    , word11 = List.head (List.drop 10 wordList) |> Maybe.withDefault ""
    , word12 = List.head (List.drop 11 wordList) |> Maybe.withDefault ""
    , word13 = List.head (List.drop 12 wordList) |> Maybe.withDefault ""
    , word14 = List.head (List.drop 13 wordList) |> Maybe.withDefault ""
    , word15 = List.head (List.drop 14 wordList) |> Maybe.withDefault ""
    , word16 = List.head (List.drop 15 wordList) |> Maybe.withDefault ""
    }


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


viewTile : String -> Element msg
viewTile word =
    Element.el
        [ Element.width (px 200)
        , Element.Background.color (rgba255 245 126 215 1)
        , Element.padding 30
        , Element.Border.rounded 12
        , Element.Font.center
        ]
        (Element.text word)


view : Model -> Element Msg
view model =
    Element.column [ Element.width Element.fill, Element.height Element.fill ]
        [ Element.column [ Element.centerY, Element.centerX, Element.spacing 24 ]
            [ Element.el [ Element.width Element.fill, Element.Font.center ] (Element.text "Connections Companion")
            , Element.wrappedRow
                [ Element.width (px 824), Element.spacing 8 ]
                (model.words |> wordsToList |> List.map viewTile)
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
