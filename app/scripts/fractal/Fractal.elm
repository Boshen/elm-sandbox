module Fractal where

import Keyboard
import Window
import String

port title : String
port title = "Elm"

axiom = "F+F+F+F"

-- model
data State = Play | Over
type Game = { state:State, s:String }
type Input = { arrows:{x:Int, y:Int} }

defaultGame : Game
defaultGame = { state=Play, s=axiom }

-- updates
stepGame : Input -> Game -> Game
stepGame {arrows} ({state, s} as game) =
  { game | s <- replace ('a', "ab") s
  }

replace (matcher, replacer) base =
  String.fromList <| concatMap match1 (String.toList base)

match c =
  if | c == 'a' -> String.toList "ab"
     | c == 'b' -> String.toList "a"
     | otherwise -> [c]

match1 c =
  if | c == 'F' -> String.toList "F+F-F-FF+F+F-F"
     --| c == 'b' -> String.toList "a"
     | otherwise -> [c]

-- display
display : (Int, Int) -> Game -> Element
display (w, h) {state, s} = asText s

-- signals
dt : Signal Time
dt = every second

input : Signal Input
input = sampleOn dt <| Input <~ Keyboard.arrows

-- main
gameState : Signal Game
gameState = foldp stepGame defaultGame input

main : Signal Element
main = display <~ Window.dimensions ~ gameState
