module Main exposing (main)

import Browser
import Html exposing (text)

main =
  Browser.sandbox
    { init = ()
    , view = \_ -> text "Hello, Elm!"
    , update = \_ model -> model
    }
