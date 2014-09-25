module Fractal where

import Keyboard
import Window

port title : String
port title = "Elm"

-- model
data State = Play | Over
type Game = { state:State }
type Input = { arrows:{x:Int, y:Int} }

defaultGame : Game
defaultGame = { state=Play }

-- updates
stepGame : Input -> Game -> Game
stepGame {arrows} ({state} as game) = game

-- display
display : (Int, Int) -> Game -> Element
display (w, h) {state} = asText state

-- signals
dt : Signal Time
dt = fps 30

input : Signal Input
input = sampleOn dt <| Input <~ Keyboard.arrows

-- main
gameState : Signal Game
gameState = foldp stepGame defaultGame input

main : Signal Element
main = display <~ Window.dimensions ~ gameState
