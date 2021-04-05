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
import List exposing (intersperse)
import Svg exposing (circle, g, polygon, svg)
import Svg.Attributes exposing (cx, cy, fill, points, r, title, viewBox)



-- MODEL


type alias Word =
    String


type alias UserAnswer =
    { submission : List Word, candidates : List { w : Word, used_up : Bool } }


type State
    = ComposingSubmission
    | YouAreRight
    | YouAreWrong


type alias Prompt =
    { prompt : List Word }


type alias Model =
    { state : State, ans : UserAnswer, prompt : Prompt }


init : Model
init =
    { prompt = { prompt = [ "chicueyi", "huan", "chicueyi" ] }
    , state =
        ComposingSubmission
    , ans =
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
    case ( msg, model.state ) of
        ( Proceed, YouAreRight ) ->
            init

        ( Proceed, YouAreWrong ) ->
            init

        ( Proceed, ComposingSubmission ) ->
            if model.ans.submission == [ "caxtolli", "huan", "ce" ] then
                { model | state = YouAreRight }

            else
                { model | state = YouAreWrong }



-- VIEW


icon : Html msg
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
            ([ icon
             , span [ title "Recording not yet available!" ] [ text "🔊" ]
             ]
                ++ (model.prompt.prompt
                        |> List.map (\w -> span [ class "annotated" ] [ text w ])
                        |> intersperse (text " ")
                   )
            )
        , div [ class "response" ]
            (model.ans.submission
                |> List.map (btn True)
                |> intersperse (text " ")
            )
        , div [ class "candidates", style "padding-bottom" "40px" ]
            (model.ans.candidates
                |> List.map candidate_button
                |> intersperse (text " ")
            )
        , submission_or_feedback model.state
        ]


btn : Bool -> String -> Html msg
btn disp txt =
    button
        [ class "word"
        , class
            (if disp then
                "displayed"

             else
                "not_displayed"
            )
        ]
        [ text txt ]


candidate_button : { a | used_up : Bool, w : String } -> Html msg
candidate_button q =
    btn (not q.used_up) q.w


submission_or_feedback : State -> Html Msg
submission_or_feedback ans =
    case ans of
        ComposingSubmission ->
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
