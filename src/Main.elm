module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html exposing (Html, b, button, div, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Svg exposing (Svg, circle, g, polygon, rect, svg)
import Svg.Attributes exposing (cx, cy, fill, height, points, r, rx, ry, title, viewBox, width, x, y)



-- MODEL


type alias Word =
    String


type UserProvidingTheAnswer
    = ComposingSubmission { submission : List Word, candidates : List { w : Word, used_up : Bool } }
    | YouAreRight
    | YouAreWrong


type alias Prompt =
    { prompt : List Word }


type alias Model =
    { ans : UserProvidingTheAnswer, prompt : Prompt }


init : Model
init =
    { prompt = { prompt = [ "chicueyi", "huan", "chicueyi" ] }
    , ans =
        ComposingSubmission
            { submission = [ "caxtolli", "huan", "ce" ]
            , candidates =
                [ { w = "huan", used_up = True }
                , { w = "caxtolli", used_up = True }
                , { w = "tlahco", used_up = False }
                , { w = "ce", used_up = True }
                , { w = "chiucnahui", used_up = False }
                , { w = "ome", used_up = False }
                ]
            }
    }



-- UPDATE


type Msg
    = Proceed -- NIQUIHTOA if ComposingSubmission; CEYOC if otherwise


update : Msg -> Model -> Model
update msg model =
    case ( msg, model.ans ) of
        ( Proceed, YouAreRight ) ->
            init

        ( Proceed, YouAreWrong ) ->
            init

        ( Proceed, ComposingSubmission submission ) ->
            if .submission submission == [ "caxtolli", "huan", "ce" ] then
                { model | ans = YouAreRight }

            else
                { model | ans = YouAreWrong }



-- VIEW


icon =
    svg [ style "width" "64px", style "height" "64px", style "vertical-align" "middle", viewBox "0 0 24 24" ]
        [ g [ title "Recording not yet available!" ]
            [ circle [ cx "12", cy "8", r "4", fill "#4b4b4b" ] []
            , circle [ cx "12", cy "26", r "12", fill "#4b4b4b" ] []
            , polygon [ fill "white", points "0,0 4,0 4,20 20,20 20,0 24,0 24,24 0,24" ] []
            ]
        ]


view : Model -> Html Msg
view model =
    div
        [ style "margin" "0 auto"
        , style "padding" "30px"
        , style "max-width" "600px"
        , style "min-width" "600px"
        ]
        [ div [ class "problem", style "font-size" "19px", style "font-weight" "sans-serif" ]
            [ icon
            , span [ title "Recording not yet available!" ] [ text "🔊" ]
            , span [ class "annotated" ] [ text "chicueyi" ]
            , text " "
            , span [ class "annotated" ] [ text "huan" ]
            , text " "
            , span [ class "annotated" ] [ text "chicueyi" ]
            ]
        , div [ class "response" ]
            [ button [ class "word", class "displayed" ] [ text "caxtolli" ]
            , text " "
            , button [ class "word", class "displayed" ] [ text "huan" ]
            , text " "
            , button [ class "word", class "displayed" ] [ text "ce" ]
            ]
        , div [ class "candidates", style "padding-bottom" "40px" ]
            [ button [ class "word", class "not_displayed" ] [ text "huan" ]
            , text " "
            , button [ class "word", class "not_displayed" ] [ text "caxtolli" ]
            , text " "
            , button [ class "word", class "displayed" ] [ text "tlahco" ]
            , text " "
            , button [ class "word", class "not_displayed" ] [ text "ce" ]
            , text " "
            , button [ class "word", class "displayed" ] [ text "chiucnahui" ]
            , text " "
            , button [ class "word", class "displayed" ] [ text "ome" ]
            ]
        , submission_or_feedback model.ans
        ]


submission_or_feedback ans =
    case ans of
        ComposingSubmission _ ->
            div [ onClick Proceed, class "submission_or_feedback" ] [ button [ class "submit", class "on_the_right" ] [ text "NIQUIHTOA" ] ]

        YouAreRight ->
            div [ onClick Proceed, class "submission_or_feedback", class "correct_feedback" ]
                [ button [ class "submit", class "on_the_right" ] [ text "CEYOC" ]
                , b [ class "feedback_text", class "correct_feedback_text" ] [ text "¡Quena!" ]
                ]

        YouAreWrong ->
            div [ onClick Proceed, class "submission_or_feedback", class "incorrect_feedback" ]
                [ button [ class "red_submit", class "on_the_right" ] [ text "CEYOC" ]
                , b [ class "feedback_text", class "incorrect_feedback_text" ] [ text "Axcanah." ]
                ]


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
