module Game where

import Keyboard
import Window

-- model
type Input = { dx:Int, dy:Int, shoot:Bool, delta: Time, window: (Int, Int)}
type Spaceship = { x:Float, y:Float }
type Bullet = { x:Float, y:Float, vx:Float, vy: Float}
type Game = { spaceship: Spaceship, bullets:[Bullet] }

defaultGame : Game
defaultGame =
  { spaceship = defaultSpaceship
  , bullets = []
  }

defaultSpaceship : Spaceship
defaultSpaceship = { x=0, y=0 }

defaultBullet x y = { x=x, y=y, vx=0, vy=0 }

-- update
stepGame : Input -> Game -> Game
stepGame {dx, dy, shoot, delta, window} ({spaceship, bullets} as game) =
  let spaceship' = updateSpaceship (dx, dy) window spaceship
      bullets' = (addBullets spaceship shoot . removeBullets window . updateBullets) bullets
  in { game | spaceship <- spaceship'
            , bullets <- bullets'
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

addBullets : Spaceship -> Bool -> [Bullet] -> [Bullet]
addBullets {x, y} shoot bullets =
  if shoot then defaultBullet x y :: bullets else bullets

removeBullets : (Int, Int) -> [Bullet] -> [Bullet]
removeBullets window bullets = filter (\b -> removeBullet window b) bullets

removeBullet : (Int, Int) -> Bullet -> Bool
removeBullet (w, h) {x, y} = y < toFloat h/2-10

updateBullets : [Bullet] -> [Bullet]
updateBullets bullets = map updateBullet bullets

updateBullet : Bullet -> Bullet
updateBullet ({x, y, vx, vy} as bullet) =
  { bullet | x <- x + vx
           , y <- y + vy
           , vx <- 0
           , vy <- 20
  }

-- display
display: (Int, Int) -> Game -> Input -> Element
display (w, h) ({spaceship, bullets} as game) input =
  collage w h
  [ debug (w,h) game input
  , drawSpaceship spaceship
  , drawBullets bullets
  ]

drawSpaceship : Spaceship -> Form
drawSpaceship {x, y} =
  ngon 3 10 |> filled black |> rotate (pi/2) |> move (x, y)

drawBullets : [Bullet] -> Form
drawBullets bullets = group <| map drawBullet bullets

drawBullet : Bullet -> Form
drawBullet {x, y} =
  circle 2 |> filled red |> move (x, y+10)

debug : (Int, Int) -> Game -> Input -> Form
debug (w, h) game input =
  let x = toFloat w/4
      y = toFloat h/4
  in group
  [ toForm (asText [game.spaceship.x, game.spaceship.y]) |> move (x, y)
  , toForm (asText [input.dx, input.dy]) |> move (x, y+20)
  , toForm (asText [fst input.window, snd input.window]) |> move (x, y+40)
  , toForm (asText (length game.bullets)) |> move (x, y+60)
  , toForm (asText input.shoot) |> move (x, y+80)
  , toForm (asText input.delta) |> move (x, y+100)
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
