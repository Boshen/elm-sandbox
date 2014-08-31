module Tracer where

import Color
import Mouse
import Window

-- model
type Input = { pos:(Int, Int), window:(Int, Int) }
type Dot = { x:Float, y:Float, radius:Float, clr:Color, alfa:Float }
type Game = { dots: [Dot], n:Int }

defaultGame : Game
defaultGame = { dots=[], n=0 }

defaultDot : Float -> Float -> Color -> Dot
defaultDot x y c = { x=x, y=y, radius=5, clr=c, alfa=1 }

-- update
stepGame : Input -> Game -> Game
stepGame input ({dots, n} as game) =
  { game | dots <- (addDot n input . removeDots . updateDots) dots
         , n <- n + 1
  }

addDot : Int -> Input -> [Dot] -> [Dot]
addDot n {pos, window} dots =
  let (x, y) = pos
      (w, h) = window
      x' = (toFloat x) - (toFloat w) / 2
      y' = -(toFloat y) + (toFloat h) / 2
      c = rgb (mkRed n) (mkGreen n) (mkBlue n)
  in defaultDot x' y' c :: dots

removeDots : [Dot] -> [Dot]
removeDots dots = filter (\d -> d.radius >= 0) dots

updateDots : [Dot] -> [Dot]
updateDots = map updateDot

updateDot : Dot -> Dot
updateDot ({radius, alfa} as dot) =
  { dot | radius <- radius - 0.1
        , alfa <- alfa - 0.02
  }

-- display
display : (Int, Int) -> Game -> Element
display (w, h) {dots} = collage w h [ drawDots dots ]

drawDots : [Dot] -> Form
drawDots = group . map drawDot

drawDot : Dot -> Form
drawDot {x, y, radius, alfa, clr} = move (x, y) <| alpha alfa <| filled clr <| circle radius

-- signals
delta = inSeconds <~ fps 30
input = sampleOn delta <| Input <~ Mouse.position ~ Window.dimensions

-- main
main = display <~ Window.dimensions ~ (foldp stepGame defaultGame input)

-- helper from http://elm-lang.org/edit/examples/Intermediate/Tracer.elm
osc n = if n <= 255 then n else (255 - (mod n 255))
c m t = osc (mod (t*m) 510)
mkRed   = c 3
mkGreen = c 5
mkBlue  = c 7
