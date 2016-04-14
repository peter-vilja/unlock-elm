module Unlock where

import Html exposing (..)
import Html.Attributes exposing (class, classList, href, style)
import Html.Events exposing (onClick)
import List exposing (length, repeat, reverse, take)
import Random exposing (generate, initialSeed, int, list)
import Signal exposing (filter, mailbox, map, merge)
import Time exposing (delay, second)

(=>) = (,)

type alias Passcode = List Int

type alias Model =
  { correct : Bool
  , guess : Passcode
  }

type Action =
  Guess Int | LastGuess Int | PassOrReset | Delete

port currentTime : Int

init : Model
init =
  { correct = False, guess = [] }

randomLength : Int
randomLength =
  initialSeed currentTime
    |> generate (int 4 6)
    |> snd
    |> generate (int 4 6)
    |> fst

correct : Passcode
correct =
  initialSeed currentTime
    |> generate (list randomLength (int 0 9))
    |> snd
    |> generate (list randomLength (int 0 9))
    |> fst

isCorrect : Passcode -> Bool
isCorrect =
  (==) correct << reverse << take (length correct)

numMb : Signal.Mailbox Action
numMb = mailbox (Guess 0)

pickLast : Action -> Bool
pickLast action =
  case action of
    LastGuess num -> True
    _ -> False

delayed : Signal Action
delayed =
  delay (0.5 * second) numMb.signal
    |> filter pickLast (Guess 0)
    |> map (\x -> PassOrReset)

actions : Signal Action
actions =
  merge numMb.signal delayed

numbers : Signal Model
numbers =
  Signal.foldp update init actions

listInit : List a -> List a
listInit list =
  take ((length list) - 1) list

update : Action -> Model -> Model
update action model =
  case action of
    Delete ->
      {model | guess = listInit model.guess}

    Guess num ->
      if (length model.guess) > ((length correct) - 1)
      then model
      else {model | guess = num :: model.guess}

    LastGuess num ->
      {model | guess = num :: model.guess}

    PassOrReset ->
      if isCorrect model.guess
      then {model | correct = True}
      else init

btnElem : (Int -> Action) -> Signal.Address Action -> Int -> String -> Html
btnElem act address num letters =
  let
    attrs =
      if num == 1 then [style ["height" => "16px"], class "letters"]
      else [class "letters"]
  in
    div
      [ onClick address (act num)
      , class "button"
      ]
      [ div [class "content"] [ div [] [text (toString num)]
                              , div attrs [text letters]
                              ]
      ]

guess : Html
guess =
  div [class "guess"] []

guessed : Html
guessed =
  div [class "guess guessed"] []

guesses : Int -> Int -> List Html
guesses n m =
  repeat m guessed ++ repeat (n-m) guess

chooseAction : List Int -> Int -> Action
chooseAction guess =
  if length guess == ((length correct) - 1)
  then LastGuess
  else Guess

passcode : Model -> Html
passcode model =
  let
    act = chooseAction model.guess
    btn = btnElem act numMb.address
    modelL = length model.guess
  in
    div [class "passcode"]
        [ div [class "enter"] [text "Enter Passcode"]
        , div [class "guesses"]
            [ div [class "list"] (guesses (length correct) modelL) ]
        , div [class "unlock"]
            [ div [] [ (btn 1 "")
                     , (btn 2 "ABC")
                     , (btn 3 "DEF")
                     ]
            , div [] [ (btn 4 "GHI")
                     , (btn 5 "JKL")
                     , (btn 6 "MNO")
                     ]
            , div [] [ (btn 7 "PQRS")
                     , (btn 8 "TUV")
                     , (btn 9 "WXYZ")
                     ]
            , div [] [btn 0 ""]
            , div
                [ onClick numMb.address Delete
                , classList [
                    ("del", True),
                    ("hide", modelL == 0)
                  ]
                ]
                [text "Delete"]
            , div [class "correct"] [text ("Psst. correct passcode is " ++ (toString correct))]
            ]
        ]

welcome : Html
welcome =
  div [class "welcome"]
      [ div [class "title"] [text "Welcome"]
      , div [class "links"]
          [ a [class "elm", href "http://elm-lang.org/"] []
          , a [class "github", href "https://github.com/peter-vilja/unlock-elm"] []
          ]
      ]

html : Model -> Html
html model =
  if model.correct
  then welcome
  else passcode model

main : Signal Html
main =
  Signal.map html numbers
