module Dodger where

import Keyboard
import Window
import Random
import Color
import Generator (..)
import Generator.Standard (..)

port title : String
port title = "Elm - Dodger"

-- model
type Input = { dx:Int
             , dy:Int
             , shoot:Bool
             , restart:Bool
             , delta:Time
             , pulse:Time
             , window: (Int, Int) }

type Position r = { r |  x:Float,  y:Float }
type Velocity r = { r | vx:Float, vy:Float }

type Spaceship = Position {}
type Bullet = Position (Velocity {})
type Meteor = Position (Velocity {radius:Float, clr:Color})

type Game = { spaceship:Spaceship
            , bullets:[Bullet]
            , meteors:[Meteor]
            , lastPulse:Maybe Time
            , isGameOver:Bool
            , gen:Generator Standard}


defaultGame : Game
defaultGame =
  { spaceship = defaultSpaceship
  , bullets = []
  , meteors = []
  , lastPulse = Nothing
  , isGameOver = False
  , gen = generator 42
  }

defaultSpaceship : Spaceship
defaultSpaceship = { x=0, y=0 }

defaultBullet : Position r -> Bullet
defaultBullet pos = { x=pos.x, y=pos.y, vx=0, vy=0 }

defaultMeteor : Position r -> Velocity r -> Float -> Color -> Meteor
defaultMeteor pos v r c = { x=pos.x, y=pos.y, vx=v.vx, vy=v.vy, radius=r, clr=c }

-- update

stepGame : Input -> Game -> Game
stepGame {dx, dy, shoot, restart, delta, pulse, window}
         ({spaceship, bullets, meteors, lastPulse, isGameOver, gen} as game) =
  let spaceship' = updateSpaceship (dx, dy) window spaceship
      bullets' = (addBullets spaceship shoot . removeBullets window . updateBullets) bullets
      meteors' = (addMeteors pulse lastPulse gen window . removeMeteors window bullets . updateMeteors) meteors
      isGameOver' = hasCollided spaceship' meteors'
      lastPulse' = updateLastPulse lastPulse pulse
      (_, gen') = float gen
  in
    if restart then defaultGame else
    { game | spaceship  <- if isGameOver then spaceship else spaceship'
            , bullets    <- if isGameOver then bullets else bullets'
            , meteors    <- if isGameOver then meteors else meteors'
            , lastPulse  <- lastPulse'
            , gen        <- gen'
            , isGameOver <- isGameOver'
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
  if shoot then defaultBullet {x=x, y=y} :: bullets else bullets

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

addMeteors : Time -> Maybe Time -> Generator g -> (Int, Int) -> [Meteor] -> [Meteor]
addMeteors pulse lastPulse g (w, h) meteors =
  let (x', g1) = float g
      (r', g2) = float g1
      (vx', g3) = float g2
      (c, g4) = float g3
      y = (toFloat h) / 2 - 10
      x = (toFloat w) / 2 - (toFloat w) * x'
      r = 10 * r' + 3
      vx = vx'
      clr = rgb 0 0 (round (255*c))
  in case lastPulse
     of Nothing -> meteors
        Just lp -> if pulse /= lp then defaultMeteor {x=x, y=y} {vx=vx', vy=0} r clr :: meteors else meteors

removeMeteors : (Int, Int) -> [Bullet] -> [Meteor] -> [Meteor]
removeMeteors window bullets meteors = filter (\m -> not <| removeMeteor window bullets m) meteors

removeMeteor : (Int, Int) -> [Bullet] -> Meteor -> Bool
removeMeteor (w, h) bullets ({x, y} as meteor) = shotMeteor bullets meteor || y <= toFloat -h/2-10

shotMeteor : [Bullet] -> Meteor -> Bool
shotMeteor bullets {x, y, radius} = or <| map (\b -> b.y >= y-radius && b.x >= (x-radius) && b.x <= (x+radius)) bullets

updateMeteors : [Meteor] -> [Meteor]
updateMeteors meteors = map updateMeteor meteors

updateMeteor : Meteor -> Meteor
updateMeteor ({x, y, vx, vy} as meteor) =
  { meteor | x <- x + vx
           , y <- y + vy
           , vx <- vx
           , vy <- -4
  }

updateLastPulse : Maybe Time -> Time -> Maybe Time
updateLastPulse lastPulse newPulse =
  case lastPulse
  of Nothing -> Just newPulse
     Just lp -> if lp /= newPulse then Just newPulse else lastPulse

hasCollided : Spaceship -> [Meteor] -> Bool
hasCollided spaceship meteors =
  not <| isEmpty <| filter (\m -> detectCollision spaceship m) meteors

detectCollision : Spaceship -> Meteor -> Bool
detectCollision spaceship meteor =
  let (x1, y1) = (spaceship.x, spaceship.y)
      (x2, y2, mr) = (meteor.x, meteor.y, meteor.radius)
  in
    ((x1-10) <= (x2) && (x2) <= (x1+10)) &&
    ((y1-10) <= (y2) && (y2) <= (y1+10))

-- display
display : (Int, Int) -> Game -> Input -> Element
display (w, h) ({spaceship, bullets, meteors, isGameOver} as game) input =
  collage w h
  [ drawSpaceship spaceship
  , drawBullets bullets
  , drawMeteors meteors
  , displayGameOver (w, h) isGameOver
  ]

drawSpaceship : Spaceship -> Form
drawSpaceship {x, y} =
  ngon 3 10 |> filled black |> rotate (pi/2) |> move (x, y)

drawBullets : [Bullet] -> Form
drawBullets bullets = group <| map drawBullet bullets

drawBullet : Bullet -> Form
drawBullet {x, y} =
  circle 2 |> filled red |> move (x, y+10)

drawMeteors : [Meteor] -> Form
drawMeteors meteors = group <| map drawMeteor meteors

drawMeteor : Meteor -> Form
drawMeteor {x, y, radius, clr} =
  circle radius |> filled clr |> move (x, y)

displayGameOver : (Int, Int) -> Bool -> Form
displayGameOver (w, h) isGameOver =
  let msg = if isGameOver then "GAME OVER\n(Press Enter)" else ""
  in plainText msg |> toForm |> move (0, toFloat h/4)

-- signals
delta = inSeconds <~ fps 60
pulse = every (millisecond)
input = sampleOn delta <| Input <~ lift .x Keyboard.arrows
                                 ~ lift .y Keyboard.arrows
                                 ~ Keyboard.space
                                 ~ Keyboard.enter
                                 ~ delta
                                 ~ pulse
                                 ~ Window.dimensions

-- main
gameState : Signal Game
gameState = foldp stepGame defaultGame input

main = display <~ Window.dimensions ~ gameState ~ input
