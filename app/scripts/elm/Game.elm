module Game where

import Keyboard
import Window

-- model
type Input = { dx:Int, dy:Int, shoot:Bool, delta: Time, window: (Int, Int)}
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
stepGame {dx, dy, shoot, delta, window} ({spaceship} as game) =
  let spaceship' = updateSpaceship (dx, dy) window spaceship
  in { game | spaceship <- spaceship'
     }

updateSpaceship : (Int, Int) -> (Int, Int) -> Spaceship -> Spaceship
updateSpaceship (dx, dy) (w, h) spaceship =
  let dx' = toFloat dx*2
      dy' = toFloat dy*2
      w' = toFloat w/2-10
      h' = toFloat h/2-10
  in { spaceship | x <- clamp -w' w' (spaceship.x + dx')
                 , y <- clamp -h' h' (spaceship.y + dy')
     }

-- display
display: (Int, Int) -> Game -> Input -> Element
display (w, h) ({spaceship} as game) input =
  collage w h
  [ debug (w,h) game input
  , drawSpaceship spaceship
  ]

drawSpaceship : Spaceship -> Form
drawSpaceship {x, y} =
  ngon 3 10 |> filled black |> rotate (pi/2) |> move (x, y)

debug : (Int, Int) -> Game -> Input -> Form
debug (w, h) game input =
  let x = toFloat w/4
      y = toFloat h/4
  in group
  [ toForm (asText [game.spaceship.x, game.spaceship.y]) |> move (x, y)
  , toForm (asText [input.dx, input.dy]) |> move (x, y+20)
  , toForm (asText [fst input.window, snd input.window]) |> move (x, y+40)
  ]

-- signals
delta = inSeconds <~ fps 60
input = sampleOn delta <| Input <~ lift .x Keyboard.arrows
                                 ~ lift .y Keyboard.arrows
                                 ~ Keyboard.space
                                 ~ delta
                                 ~ Window.dimensions

-- main
gameState : Signal Game
gameState = foldp stepGame defaultGame input

main = display <~ Window.dimensions ~ gameState ~ input
