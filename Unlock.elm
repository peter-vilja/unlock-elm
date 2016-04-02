module Unlock where

import Html exposing (..)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onClick)
import Signal exposing (filter, mailbox, map, merge)
import List exposing (length, repeat, reverse, take)
import Time exposing (delay, second)

(=>) = (,)

type alias Passcode = List Int

type alias Model =
  { correct : Bool
  , guess : Passcode
  }

type Action = Guess Int | LastGuess Int | PassOrReset

init : Model
init =
  { correct = False, guess = [] }

correct : Passcode
correct =
  [1, 2, 3, 4]

isCorrect : Passcode -> Bool
isCorrect =
  (==) correct << reverse << take 4

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

update : Action -> Model -> Model
update action model =
  case action of
    Guess num ->
      {model | guess = num :: model.guess}

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
      [ div [class "content"]
          [ div [] [text (toString num)]
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
  if length guess == 3
  then LastGuess
  else Guess

passcode : Model -> Html
passcode model =
  let
    act = chooseAction model.guess
    btn = btnElem act numMb.address
  in
    div [class "passcode"]
        [ div [class "enter"] [text "Enter Passcode"]
        , div [class "guesses"]
            [ div [class "list"] (guesses (length correct) (length model.guess)) ]
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
