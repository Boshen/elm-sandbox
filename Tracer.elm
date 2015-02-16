module Tracer where

import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import List
import Mouse
import Signal (..)
import Time (..)
import Window

port title : String
port title = "Elm - Mouse Tracer"

-- model
type alias Input = { pos:(Int, Int), click:Bool, window:(Int, Int) }
type alias Dot = { x:Float, y:Float, radius:Float, clr:Color, alfa:Float, moveXY: (Float, Float)->(Float, Float) }
type alias Game = { dots: List Dot, n:Int }

defaultGame : Game
defaultGame = { dots=[], n=0 }

defaultDot : Float -> Float -> Color -> ((Float, Float) -> (Float, Float)) -> Dot
defaultDot x y c mv = { x=x, y=y, radius=5, clr=c, alfa=1, moveXY=mv }

-- update
stepGame : Input -> Game -> Game
stepGame input ({dots, n} as game) =
  { game | dots <- (addDots n input >> removeDots >> updateDots) dots
         , n <- n + 1
  }

addDots : Int -> Input -> List Dot -> List Dot
addDots n {pos, click, window} dots =
  let (x, y) = pos
      (w, h) = window
      x' = (toFloat x) - (toFloat w) / 2
      y' = -(toFloat y) + (toFloat h) / 2
      c = rgb (mkRed n) (mkGreen n) (mkBlue n)
      moreDots = if click then List.map (createClickedDot x' y' c) (coordinates 5 12) else [defaultDot x' y' c identity]
  in dots ++ moreDots

removeDots : List Dot -> List Dot
removeDots dots = List.filter (\d -> d.radius >= 0) dots

updateDots : List Dot -> List Dot
updateDots = List.map updateDot

updateDot : Dot -> Dot
updateDot ({x, y, radius, alfa, moveXY} as dot) =
  let (x', y') = moveXY(x, y)
  in { dot | radius <- radius - 0.1
           , alfa <- alfa - 0.02
           , x <- x'
           , y <- y'
     }

createClickedDot : Float -> Float -> Color -> ((Float, Float) -> Dot)
createClickedDot x y c = (\(dx, dy)-> defaultDot x y c (\(x', y') -> (x'+dx, y'+dy)))

-- display
display : (Int, Int) -> Game -> Element
display (w, h) {dots} = collage w h [ drawDots dots ]

drawDots : List Dot -> Form
drawDots = group << List.map drawDot

drawDot : Dot -> Form
drawDot {x, y, radius, alfa, clr} = move (x, y) <| alpha alfa <| filled clr <| circle radius

-- signals
delta = inSeconds <~ fps 30
input = sampleOn delta <| Input <~ Mouse.position ~ Mouse.isDown ~ Window.dimensions

-- main
main : Signal Element
main = display <~ Window.dimensions ~ (foldp stepGame defaultGame input)

-- a list of co-ordinates to draw
coordinates : Float -> Float -> List (Float, Float)
coordinates radius len =
  let g n = fromPolar (radius, turns (n/len))
  in List.map g [0..len]

-- helper from http://elm-lang.org/edit/examples/Intermediate/Tracer.elm
osc n = if n <= 255 then n else (255 - (n % 255))
c m t = osc ((t*m) % 510)
mkRed   = c 3
mkGreen = c 5
mkBlue  = c 7
