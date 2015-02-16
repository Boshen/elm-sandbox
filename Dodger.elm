module Dodger where

import Color (..)
import Keyboard
import List
import Random
import Window
import Time (..)
import Text (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Signal (..)

port title : String
port title = "Elm - Dodger"

-- model
type alias Input = { dx:Int
             , dy:Int
             , shoot:Bool
             , restart:Bool
             , delta:Time
             , pulse:Time
             , window: (Int, Int) }

type alias Position r = { r |  x:Float,  y:Float }
type alias Velocity r = { r | vx:Float, vy:Float }

type alias Spaceship = Position {}
type alias Bullet = Position (Velocity {})
type alias Meteor = Position (Velocity {radius:Float, clr:Color})

type alias Game =
  { spaceship:Spaceship
  , bullets: List Bullet
  , meteors: List Meteor
  , lastPulse:Maybe Time
  , isGameOver:Bool
  , seed:Random.Seed
  }

defaultGame : Game
defaultGame =
  { spaceship = defaultSpaceship
  , bullets = []
  , meteors = []
  , lastPulse = Nothing
  , isGameOver = False
  , seed = Random.initialSeed 42
  }

defaultSpaceship : Spaceship
defaultSpaceship = { x=0, y=0 }

defaultBullet : Position r -> Bullet
defaultBullet pos = { x=pos.x, y=pos.y, vx=0, vy=0 }

defaultMeteor : Position r -> Velocity r -> Float -> Color -> Meteor
defaultMeteor pos v r c = { x=pos.x, y=pos.y, vx=v.vx, vy=v.vy, radius=r, clr=c }

genFloat = Random.generate (Random.float 0 1)

-- update

stepGame : Input -> Game -> Game
stepGame {dx, dy, shoot, restart, delta, pulse, window}
         ({spaceship, bullets, meteors, lastPulse, isGameOver, seed} as game) =
  let spaceship' = updateSpaceship (dx, dy) window spaceship
      bullets' = (addBullets spaceship shoot >> removeBullets window >> updateBullets) bullets
      meteors' = (addMeteors pulse lastPulse seed window >> removeMeteors window bullets >> updateMeteors) meteors
      isGameOver' = hasCollided spaceship' meteors'
      lastPulse' = updateLastPulse lastPulse pulse
  in
    if restart then defaultGame else
    { game | spaceship  <- if isGameOver then spaceship else spaceship'
            , bullets    <- if isGameOver then bullets else bullets'
            , meteors    <- if isGameOver then meteors else meteors'
            , lastPulse  <- lastPulse'
            , isGameOver <- isGameOver'
            , seed <- snd <| genFloat seed
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

addBullets : Spaceship -> Bool -> List Bullet -> List Bullet
addBullets {x, y} shoot bullets =
  if shoot then defaultBullet {x=x, y=y} :: bullets else bullets

removeBullets : (Int, Int) -> List Bullet -> List Bullet
removeBullets window bullets = List.filter (\b -> removeBullet window b) bullets

removeBullet : (Int, Int) -> Bullet -> Bool
removeBullet (w, h) {x, y} = y < toFloat h/2-10

updateBullets : List Bullet -> List Bullet
updateBullets bullets = List.map updateBullet bullets

updateBullet : Bullet -> Bullet
updateBullet ({x, y, vx, vy} as bullet) =
  { bullet | x <- x + vx
           , y <- y + vy
           , vx <- 0
           , vy <- 20
  }

addMeteors : Time -> Maybe Time -> Random.Seed -> (Int, Int) -> List Meteor -> List Meteor
addMeteors pulse lastPulse seed (w, h) meteors =
  let (x', seed1) = genFloat seed
      (r', seed2) = genFloat seed1
      (vx', seed3) = genFloat seed2
      (c, _) = genFloat seed3
      y = (toFloat h) / 2 - 10
      x = (toFloat w) / 2 - (toFloat w) * x'
      r = 10 * r' + 3
      vx = vx'
      clr = rgb 0 0 (round (255*c))
  in case lastPulse
     of Nothing -> meteors
        Just lp -> if pulse /= lp then defaultMeteor {x=x, y=y} {vx=vx', vy=0} r clr :: meteors else meteors

removeMeteors : (Int, Int) -> List Bullet -> List Meteor -> List Meteor
removeMeteors window bullets meteors = List.filter (\m -> not <| removeMeteor window bullets m) meteors

removeMeteor : (Int, Int) -> List Bullet -> Meteor -> Bool
removeMeteor (w, h) bullets ({x, y} as meteor) = shotMeteor bullets meteor || y <= toFloat -h/2-10

shotMeteor : List Bullet -> Meteor -> Bool
shotMeteor bullets {x, y, radius} = List.any  (\b -> b.y >= y-radius && b.x >= (x-radius) && b.x <= (x+radius)) bullets

updateMeteors : List Meteor -> List Meteor
updateMeteors meteors = List.map updateMeteor meteors

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

hasCollided : Spaceship -> List Meteor -> Bool
hasCollided spaceship meteors =
  not <| List.isEmpty <| List.filter (\m -> detectCollision spaceship m) meteors

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

drawBullets : List Bullet -> Form
drawBullets bullets = group <| List.map drawBullet bullets

drawBullet : Bullet -> Form
drawBullet {x, y} =
  circle 2 |> filled red |> move (x, y+10)

drawMeteors : List Meteor -> Form
drawMeteors meteors = group <| List.map drawMeteor meteors

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
input = sampleOn delta <| Input <~ map .x Keyboard.arrows
                                 ~ map .y Keyboard.arrows
                                 ~ Keyboard.space
                                 ~ Keyboard.enter
                                 ~ delta
                                 ~ pulse
                                 ~ Window.dimensions

-- main
gameState : Signal Game
gameState = foldp stepGame defaultGame input

main = display <~ Window.dimensions ~ gameState ~ input
