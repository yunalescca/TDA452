-- |  Haste
-- An example to introduce web programming with Haste
-- Functional Programming course 2017.
-- Thomas Hallgren

import Haste.DOM
import Haste.Events

main = do hello <- newTextElem "Hello"
          header <- newElem "h1"
          appendChild header hello
          appendChild documentBody header

          input <- newElem "input"
          button <- newElem "input"
                    `with` [attr "type" =: "button",
                            attr "value" =: "Click me"]

          appendChild documentBody input
          appendChild documentBody button

          let incr _ = do s <- getProp input "value"
                          let n = read s :: Int -- can fail!
                          setProp input "value" (show (n+1))

          onEvent button Click incr
