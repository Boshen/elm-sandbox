module Snake where

import Keyboard
import Random
import Window

port title : String
port title = "Elm - Snake"

-- model
data State = Play | Over
type Position r = { r | x:Int, y:Int }
type Velocity r = { r | vx:Int, vy:Int }
type Snake = Position (Velocity {body:[Position {}]})
type Food = Position {}
type Game = { snake:Snake, food:Food, state:State }
type Input = { dx:Int
             , dy:Int
             , window:(Int, Int)
             , restart:Bool
             , rand:(Float, Float)}

gridSize = 10

defaultGame : Game
defaultGame = { snake=defaultSnake, food={x=10, y=10}, state=Play }

defaultSnake : Snake
defaultSnake = { x=1, y=0, vx=1, vy=0, body=(repeat 10 {x=0,y=0}) }

-- updates
stepGame : Input -> Game -> Game
stepGame {dx, dy, window, restart, rand} ({snake, food, state} as game) =
  case state of
    Play ->
      let eaten = snake.x == food.x && snake.y == food.y
          snake' = (stepSnake (dx, dy) window) . (eatFood eaten food) <| snake
          food' = updateFood eaten window rand food
          state' = updateState window snake
      in
        { game | snake <- snake'
               , food <- food'
               , state <- state'
        }
    Over ->
      if restart then defaultGame else game

stepSnake : (Int, Int) -> (Int, Int) -> Snake -> Snake
stepSnake (dx, dy) (w, h) ({x, y, vx, vy, body} as snake) =
  let (vx', vy') = if | dx /= 0 && vx == 0 -> (dx,  0)
                      | dy /= 0 && vy == 0 -> ( 0, dy)
                      | otherwise          -> (vx, vy)
  in { snake | x <- clamp (div -w 2) (div w 2) (x + vx)
             , y <- clamp (div -h 2) (div h 2) (y + vy)
             , vx <- vx'
             , vy <- vy'
             , body <- {x=x, y=y} :: take ((length body)-1) body
     }

eatFood : Bool -> Food -> Snake -> Snake
eatFood eaten food snake =
  if eaten then {snake | body <- food :: snake.body} else snake

updateFood : Bool -> (Int, Int) -> (Float, Float) -> Food -> Food
updateFood eaten (w, h) (rx, ry) food =
  if eaten then
     { food | x <- round <| (rx-0.5)*(toFloat w)
            , y <- round <| (ry-0.5)*(toFloat h)
     }
  else food

updateState : (Int, Int) -> Snake -> State
updateState (w, h) {x, y, body} =
  if | abs x == div w 2 || abs y == div h 2 -> Over
     | any ((==) {x=x, y=y}) body -> Over
     | otherwise -> Play

-- display
display : (Int, Int) -> Game -> Element
display (w, h) {snake, food} =
  collage w h [ draw black snake
              , draw red food
              , group <| map (draw black) snake.body
              ]

draw : Color -> Position r -> Form
draw clr {x, y} = move (toFloat x*gridSize, toFloat y*gridSize)
               <| filled clr
               <| square gridSize

-- signals
dt : Signal Time
dt = fps 30

randWH : Signal (Float, Float)
randWH = lift2 (,) (Random.float dt) (Random.float dt)

input : Signal Input
input = sampleOn dt <| Input <~ lift .x Keyboard.arrows
                              ~ lift .y Keyboard.arrows
                              ~ lift (\(w, h) -> ((div w gridSize), (div h gridSize))) Window.dimensions
                              ~ Keyboard.space
                              ~ randWH

-- main
gameState : Signal Game
gameState = foldp stepGame defaultGame input

main : Signal Element
main = display <~ Window.dimensions ~ gameState
