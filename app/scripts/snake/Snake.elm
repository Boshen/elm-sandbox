module Snake where

import Keyboard
import Window

port title : String
port title = "Elm - Snake"

type Position r = { r | x:Int, y:Int }
type Velocity r = { r | vx:Int, vy:Int }
type Snake = Position (Velocity {body:[Position {}]})
type Food = Position {}
type Game = { snake:Snake, food:Food }

type Input = { dx:Int
             , dy:Int
             , window:(Int, Int) }

gridSize = 10

defaultGame : Game
defaultGame = { snake=defaultSnake, food={x=10, y=10}}

defaultSnake : Snake
defaultSnake = { x=0, y=0, vx=0, vy=0, body=defaultBody }

defaultBody = map (\x -> {x=x,y=0}) [0..10]

stepGame : Input -> Game -> Game
stepGame {dx, dy, window} ({snake} as game) =
  { game | snake <- stepSnake (dx, dy) window snake }

stepSnake : (Int, Int) -> (Int, Int) -> Snake -> Snake
stepSnake (dx, dy) (w, h) ({x, y, vx, vy, body} as snake) =
  let (vx', vy') = if | dx /= 0 && vx == 0 -> (dx, 0)
                      | dy /= 0 && vy == 0 -> (0, dy)
                      | otherwise          -> (vx, vy)
  in { snake | x <- clamp (div -w 2) (div w 2) (x + vx)
          , y <- clamp (div -h 2) (div h 2) (y + vy)
          , vx <- vx'
          , vy <- vy'
          , body <- {x=x,y=y} :: take ((length body)-1) body
     }

display : (Int, Int) -> Game -> Element
display (w, h) {snake, food} =
  collage w h [ draw black snake
              , group <| map (draw gray) snake.body
              , draw red food
              , debug (w, h) snake
              ]

draw : Color -> Position r -> Form
draw clr {x, y} = move (toFloat x*gridSize, toFloat y*gridSize)
                      <| filled clr
                      <| square gridSize

debug (w, h) {x, y, vx, vy} =
  group [ move (toFloat w/4, toFloat h/4-20) <| toForm <| asText (w, h)
        , move (toFloat w/4, toFloat h/4) <| toForm <| asText (x, y, vx, vy)
        ]

dt : Signal Time
dt = fps 60

input : Signal Input
input = sampleOn dt <| Input <~ lift .x Keyboard.arrows
                              ~ lift .y Keyboard.arrows
                              ~ lift (\(w, h) -> ((div w gridSize), (div h gridSize))) Window.dimensions

gameState : Signal Game
gameState = foldp stepGame defaultGame input

main : Signal Element
main = display <~ Window.dimensions ~ gameState
