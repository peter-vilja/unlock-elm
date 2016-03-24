import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Signal exposing (mailbox)
import List exposing (take, reverse)

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

btnElem : Signal.Address Int -> Int -> Html
btnElem address num =
  div
    [ onClick address num
    , style [ "padding" => "35px 40px"
            , "display" => "inline-block"
            , "border" => "1px solid #eee"
            , "border-radius" => "50px"
            , "margin" => "5px"
            , "cursor" => "pointer"
            , "font-family" => "Helvetica"
            , "-webkit-user-select" => "none"
            ]
    ]
    [text (toString num)]

isCorrect : Model -> Bool
isCorrect =
  (==) correct << reverse << take 4

html : Model -> Html
html model =
  let
    passCorrect = (isCorrect model)
    btn = btnElem numberMailbox.address
  in
    div []
          [ div [] [ (btn 1)
                   , (btn 2)
                   , (btn 3)
                   ]
          , div [] [ (btn 4)
                   , (btn 5)
                   , (btn 6)
                   ]
          , div [] [ (btn 7)
                   , (btn 8)
                   , (btn 9)
                   ]
          , div [] [text (toString model)]
          , div [] [text (toString passCorrect)]
          ]

main : Signal Html
main =
  Signal.map html numbers
