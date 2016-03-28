module Unlock where

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Signal exposing (mailbox)
import List exposing (length, repeat, reverse, take)

(=>) = (,)

type alias Model = List Int

correct : List Int
correct =
  [1, 2, 3, 4]

init : Model
init = []

numberMailbox : Signal.Mailbox Int
numberMailbox = mailbox 0

numbers : Signal Model
numbers =
  Signal.foldp update init numberMailbox.signal

update : Int -> Model -> Model
update = (::)

btnElem : Signal.Address Int -> Int -> String -> Html
btnElem address num letters =
  let
    attrs =
      if num == 1 then [style ["height" => "16px"], class "letters"]
      else [class "letters"]
  in
    div
      [ onClick address num
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

guesses : Int -> List Html
guesses n =
  repeat n guess

isCorrect : Model -> Bool
isCorrect =
  (==) correct << reverse << take 4

html : Model -> Html
html model =
  let
    passCorrect = (isCorrect model)
    btn = btnElem numberMailbox.address
    guessClass = "list " ++ "guessed-" ++ (toString (length model))
  in
    div [class "passcode"]
        [ div [class "enter"] [text "Enter Passcode"]
        , div [class "guesses"]
            [ div [class guessClass] (guesses (length correct))
            ]
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

main : Signal Html
main =
  Signal.map html numbers
