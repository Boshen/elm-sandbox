module Snake where

import Color (..)
import Keyboard
import Random
import Window
import List
import Graphics.Collage (..)
import Graphics.Element (..)
import Time (..)
import Signal (..)

port title : String
port title = "Elm - Snake"

-- model
type State = Play | Over
type alias Position r = { r | x:Int, y:Int }
type alias Velocity r = { r | vx:Int, vy:Int }
type alias Snake = Position (Velocity {body: List (Position {})})
type alias Food = Position {}
type alias Game = { snake:Snake, food:Food, state:State, seed:Random.Seed }
type alias Input =
  { dx:Int
  , dy:Int
  , window:(Int, Int)
  , restart:Bool
  }

gridSize = 10

defaultGame : Game
defaultGame = { snake=defaultSnake, food={x=10, y=10}, state=Play, seed=Random.initialSeed 42 }

defaultSnake : Snake
defaultSnake = { x=1, y=0, vx=1, vy=0, body=(List.repeat 10 {x=0,y=0}) }

genPair = Random.generate <| Random.pair (Random.float 0 1) (Random.float 0 1)

-- updates
stepGame : Input -> Game -> Game
stepGame {dx, dy, window, restart} ({snake, food, state, seed} as game) =
  case state of
    Play ->
      let eaten = snake.x == food.x && snake.y == food.y
          snake' = (stepSnake (dx, dy) window) >> (eatFood eaten food) <| snake
          food' = updateFood eaten window seed food
          state' = updateState window snake
      in
        { game | snake <- snake'
               , food <- food'
               , state <- state'
               , seed <- snd <| genPair seed
        }
    Over ->
      if restart then defaultGame else game

stepSnake : (Int, Int) -> (Int, Int) -> Snake -> Snake
stepSnake (dx, dy) (w, h) ({x, y, vx, vy, body} as snake) =
  let (vx', vy') = if | dx /= 0 && vx == 0 -> (dx,  0)
                      | dy /= 0 && vy == 0 -> ( 0, dy)
                      | otherwise          -> (vx, vy)
  in { snake | x <- clamp (-w // 2) (w // 2) (x + vx)
             , y <- clamp (-h // 2) (h // 2) (y + vy)
             , vx <- vx'
             , vy <- vy'
             , body <- {x=x, y=y} :: List.take ((List.length body)-1) body
     }

eatFood : Bool -> Food -> Snake -> Snake
eatFood eaten food snake =
  if eaten then {snake | body <- food :: snake.body} else snake

updateFood : Bool -> (Int, Int) -> Random.Seed -> Food -> Food
updateFood eaten (w, h) seed food =
  let ((rx, ry), _) = genPair seed in
  if eaten then
     { food | x <- round <| (rx-0.5)*(toFloat w)
            , y <- round <| (ry-0.5)*(toFloat h)
     }
  else food

updateState : (Int, Int) -> Snake -> State
updateState (w, h) {x, y, body} =
  if | abs x == (w // 2) || abs y == (h // 2) -> Over
     | List.any ((==) {x=x, y=y}) body -> Over
     | otherwise -> Play

-- display
display : (Int, Int) -> Game -> Element
display (w, h) {snake, food} =
  collage w h [ draw black snake
              , draw red food
              , group <| List.map (draw black) snake.body
              ]

draw : Color -> Position r -> Form
draw clr {x, y} = move (toFloat x*gridSize, toFloat y*gridSize)
               <| filled clr
               <| square gridSize

-- signals
dt : Signal Time
dt = fps 30

input : Signal Input
input = sampleOn dt <| Input <~ map .x Keyboard.arrows
                              ~ map .y Keyboard.arrows
                              ~ map (\(w, h) -> ((w // gridSize), (h //gridSize))) Window.dimensions
                              ~ Keyboard.space

-- main
gameState : Signal Game
gameState = foldp stepGame defaultGame input

main : Signal Element
main = display <~ Window.dimensions ~ gameState
