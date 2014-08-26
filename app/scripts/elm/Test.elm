module Test where

import Keyboard
import Window

ball : { x:Float, y:Float, angle: Float}
ball = { x=0, y=0, angle=90}

step (t, {x, y}) ball =
  let x' = toFloat x
      y' = toFloat y
      angle' = ball.angle + y'
  in { ball | x <- ball.x + x'
            , angle <- if | angle' <= 0 -> 0 
                          | angle' >= 180 -> 180
                          | otherwise -> angle'
      }

delta = inSeconds <~ fps 25

input = sampleOn delta (lift2 (,) delta Keyboard.arrows)

display (w, h) {x, y, angle} = 
  let dx = 10*cos(degrees (180-angle))
      dy = 10*sin(degrees (180-angle))
  in collage w h
    [ move (toFloat w/4, toFloat h/4) <| toForm (asText [x, y, angle])
    , move (x, y) <| filled black (rect 20 5)
    , move (x+dx, y+dy) <| rotate (degrees (90-angle)) <| filled black (rect 2 20)
    ]

main = lift2 display Window.dimensions (foldp step ball input)
