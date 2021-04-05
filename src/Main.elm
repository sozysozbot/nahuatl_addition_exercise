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


type alias CandidatePossiblyUsedUp =
    { w : Word, used_up : Bool }


type alias UserAnswer =
    { response : List Word, candidates : List CandidatePossiblyUsedUp }


type State
    = ComposingSubmission
    | YouAreRight
    | YouAreWrong


type alias Model =
    { state : State, ans : UserAnswer, prompt : List Word }


init : Model
init =
    { prompt = [ "chicueyi", "huan", "chicueyi" ]
    , state =
        ComposingSubmission
    , ans =
        { response = []
        , candidates =
            [ { w = "huan", used_up = False }
            , { w = "caxtolli", used_up = False }
            , { w = "tlahco", used_up = False }
            , { w = "ce", used_up = False }
            , { w = "chiucnahui", used_up = False }
            , { w = "ome", used_up = False }
            ]
        }
    }



-- UPDATE


type Msg
    = Proceed -- NIQUIHTOA if ComposingSubmission; CEYOC if otherwise
    | UpdateAnsAs UserAnswer


update : Msg -> Model -> Model
update msg model =
    case ( msg, model.state ) of
        ( Proceed, YouAreRight ) ->
            init

        ( Proceed, YouAreWrong ) ->
            init

        ( Proceed, ComposingSubmission ) ->
            if model.ans.response == [ "caxtolli", "huan", "ce" ] then
                { model | state = YouAreRight }

            else
                { model | state = YouAreWrong }

        ( UpdateAnsAs new_ans, ComposingSubmission ) ->
            { model | ans = new_ans }

        {- should not happen! -}
        ( UpdateAnsAs _, YouAreRight ) ->
            model

        {- should not happen! -}
        ( UpdateAnsAs _, YouAreWrong ) ->
            model


icon : Html msg
icon =
    svg [ style "width" "64px", style "height" "64px", style "vertical-align" "middle", viewBox "0 0 24 24" ]
        [ g [ title "Recording not yet available!" ]
            [ circle [ cx "12", cy "8", r "4", fill "#4b4b4b" ] []
            , circle [ cx "12", cy "26", r "12", fill "#4b4b4b" ] []
            , polygon [ fill "white", points "0,0 4,0 4,20 20,20 20,0 24,0 24,24 0,24" ] []
            ]
        ]


resurrect_word : String -> List CandidatePossiblyUsedUp -> List CandidatePossiblyUsedUp
resurrect_word word list =
    case list of
        [] ->
            []

        x :: xs ->
            if not x.used_up then
                x :: resurrect_word word xs

            else if x.w == word then
                { w = x.w, used_up = False } :: xs

            else
                x :: resurrect_word word xs


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
                ++ (model.prompt
                        |> List.map (\w -> span [ class "annotated" ] [ text w ])
                        |> intersperse (text " ")
                   )
            )
        , div [ class "response" ]
            (model.ans.response
                |> all_possible_removal_of_one_elem
                |> List.map
                    (\( word_to_be_removed_from_response, remaining ) ->
                        UpdateAnsAs
                            { response = remaining
                            , candidates = resurrect_word word_to_be_removed_from_response model.ans.candidates
                            }
                            |> btn True word_to_be_removed_from_response
                    )
                |> intersperse (text " ")
            )
        , div [ class "candidates", style "padding-bottom" "40px" ]
            (model.ans.candidates
                |> all_possible_turnoff_of_one_elem
                |> List.map
                    (\( q, new_candidates ) ->
                        UpdateAnsAs { candidates = new_candidates, response = model.ans.response ++ [ q.w ] }
                            |> btn (not q.used_up) q.w
                    )
                |> intersperse (text " ")
            )
        , html_for_submission_or_feedback model.state
        ]


all_possible_turnoff_of_one_elem : List CandidatePossiblyUsedUp -> List ( CandidatePossiblyUsedUp, List CandidatePossiblyUsedUp )
all_possible_turnoff_of_one_elem list =
    case list of
        [] ->
            []

        x :: xs ->
            ( x, { x | used_up = True } :: xs )
                :: helper x (all_possible_turnoff_of_one_elem xs)


helper : a -> List ( b, List a ) -> List ( b, List a )
helper x =
    List.map (\( b, a ) -> ( b, x :: a ))



{-
   all_possible_removal_of_one_elem [0,1,2,3,4] == [(0,[1,2,3,4]),(1,[0,2,3,4]),(2,[0,1,3,4]),(3,[0,1,2,4]),(4,[0,1,2,3])]
-}


all_possible_removal_of_one_elem : List b -> List ( b, List b )
all_possible_removal_of_one_elem list =
    case list of
        [] ->
            []

        z :: zs ->
            ( z, zs ) :: helper z (all_possible_removal_of_one_elem zs)


btn : Bool -> String -> msg -> Html msg
btn disp txt msg =
    if disp then
        button
            [ class "word"
            , class "displayed"
            , onClick msg
            ]
            [ text txt ]

    else
        button
            [ class "word"
            , class "not_displayed"
            ]
            [ text txt ]


html_for_submission_or_feedback : State -> Html Msg
html_for_submission_or_feedback ans =
    case ans of
        ComposingSubmission ->
            div [ class "submission_or_feedback" ] [ button [ onClick Proceed, class "submit", class "on_the_right" ] [ text "NIQUIHTOA" ] ]

        YouAreRight ->
            div [ class "submission_or_feedback", class "correct_feedback" ]
                [ button [ onClick Proceed, class "submit", class "on_the_right" ] [ text "CEYOC" ]
                , b [ class "feedback_text", class "correct_feedback_text" ] [ text "¡Quena!" ]
                ]

        YouAreWrong ->
            div [ class "submission_or_feedback", class "incorrect_feedback" ]
                [ button [ onClick Proceed, class "red_submit", class "on_the_right" ] [ text "CEYOC" ]
                , b [ class "feedback_text", class "incorrect_feedback_text" ] [ text "Axcanah." ]
                ]


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
