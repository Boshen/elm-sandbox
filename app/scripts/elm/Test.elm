module Test where

import Keyboard
import Window

ball : { x:Float, y:Float }
ball = { x=0, y=0 }

step (t, {x, y}) ball =
  let x' = toFloat x
      y' = toFloat y
  in { ball | x <- ball.x + x'
            , y <- ball.y + y' }

delta = inSeconds <~ fps 25

input = sampleOn delta (lift2 (,) delta Keyboard.arrows)

display (w, h) {x, y} = collage w h
  [ move (toFloat w/4, toFloat h/4) <| toForm (asText [x, y])
  , move (x, y) <| filled black (circle 5)
  ]

main = lift2 display Window.dimensions (foldp step ball input)
