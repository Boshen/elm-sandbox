module Tracer where

import Color
import Mouse
import Time
import Window

-- model
type Input = { pos:(Float, Float), window:(Float, Float), time:Time }
type Dot = { x:Float, y:Float, radius:Float, clr:Color, alfa:Float }

type Game = { dots: [Dot], n:Int }

defaultGame : Game
defaultGame = { dots=[], n=0 }

defaultDot : Float -> Float -> Color -> Dot
defaultDot x y c = { x=x, y=y, radius=5, clr=c, alfa=1}

-- update
stepGame : Input -> Game -> Game
stepGame input ({dots, n} as game) =
  let dots' = (addDot n input . removeDot . updateDots) dots
  in { game | dots <- dots'
            , n <- n + 1
     }

addDot : Int -> Input -> [Dot] -> [Dot]
addDot n {pos, window, time} dots =
  let (x, y) = pos
      (w, h) = window
      x' = x - w / 2
      y' = -y + h / 2
      c = rgb (mkRed n) (mkGreen n) (mkBlue n)
  in defaultDot x' y' c :: dots

removeDot : [Dot] -> [Dot]
removeDot dots = filter (\d -> d.radius >= 0) dots

updateDots : [Dot] -> [Dot]
updateDots = map updateDot

updateDot : Dot -> Dot
updateDot ({radius, alfa} as dot) =
  { dot | radius <- radius - 0.1
        , alfa <- alfa - 0.02
  }

-- display
display : Game -> Input -> Element
display {dots} input =
  let (w, h) = input.window
  in collage (round w) (round h)
    [ drawDots dots ]

drawDots : [Dot] -> Form
drawDots = group . map drawDot

drawDot : Dot -> Form
drawDot {x, y, radius, alfa, clr} = move (x, y) <| alpha alfa <| filled clr <| circle radius

-- signals
delta = inSeconds <~ fps 30
input = sampleOn delta <| Input <~ lift (\(x,y)->(toFloat x, toFloat y)) Mouse.position
                                 ~ lift (\(w,h)->(toFloat w, toFloat h)) Window.dimensions
                                 ~ (foldp (+) 0 (fps 30))

-- main
gameState : Signal Game
gameState = foldp stepGame defaultGame input

main = display <~ gameState ~ input

-- helper from http://elm-lang.org/edit/examples/Intermediate/Tracer.elm
osc n = if n <= 255 then n else (255 - (n `mod` 255))
c m t = osc ((t*m) `mod` 510)
mkRed   = c 3
mkGreen = c 5
mkBlue  = c 7
