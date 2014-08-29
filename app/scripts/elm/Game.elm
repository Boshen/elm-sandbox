module Game where

import Keyboard
import Window

-- model
type Input = { x:Int, y:Int, space:Bool, delta: Time }
type Spaceship = { x:Float, y:Float }
type Game = { spaceship: Spaceship }

defaultGame : Game
defaultGame =
  { spaceship = defaultSpaceship
  }

defaultSpaceship : Spaceship
defaultSpaceship = { x=0, y=0 }

-- update
stepGame : Input -> Game -> Game
stepGame input game = game

-- display
display: (Int, Int) -> Game -> Input -> Element
display (w, h) game input =
  collage w h
  [ debug (w,h) game input
  , drawSpaceship game
  ]

drawSpaceship : Game -> Form
drawSpaceship game =
  ngon 3 10 |> filled black |> rotate (pi/2)

debug : (Int, Int) -> Game -> Input -> Form
debug (w, h) game input =
  let x = toFloat w/4
      y = toFloat h/4
  in group
  [ toForm (asText [game.spaceship.x, game.spaceship.y]) |> move (x, y)
  , toForm (asText [input.x, input.y]) |> move (x, y+20)
  ]

-- signals
delta = inSeconds <~ fps 25
input = sampleOn delta <| Input <~ lift .x Keyboard.arrows
                                 ~ lift .y Keyboard.arrows
                                 ~ Keyboard.space
                                 ~ delta

-- main
gameState : Signal Game
gameState = foldp stepGame defaultGame input

main = display <~ Window.dimensions ~ gameState ~ input
