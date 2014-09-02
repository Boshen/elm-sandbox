module Space where

import Debug
import Keyboard
import Window
import Random
import Color
import Generator (..)
import Generator.Standard (..)

-- model
type Input = { dx:Int
             , dy:Int
             , shoot:Bool
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
            , gen:Generator Standard}


defaultGame : Game
defaultGame =
  { spaceship = defaultSpaceship
  , bullets = []
  , meteors = []
  , lastPulse = Nothing
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
stepGame {dx, dy, shoot, delta, pulse, window}
         ({spaceship, bullets, meteors, lastPulse, gen} as game) =
  let spaceship' = updateSpaceship (dx, dy) window spaceship
      bullets' = (addBullets spaceship shoot . removeBullets window . updateBullets) bullets
      meteors' = (addMeteors pulse lastPulse gen window . removeMeteors window . updateMeteors) meteors
      lastPulse' = updateLastPulse lastPulse pulse
      (_, gen') = float gen
  in { game | spaceship <- spaceship'
            , bullets <- bullets'
            , meteors <- meteors'
            , lastPulse <- lastPulse'
            , gen <- gen'
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
      r = 3 * r' + 3
      vx = vx'
      clr = rgb 0 0 (round (255*c))
  in case lastPulse
     of Nothing -> meteors
        Just lp -> if pulse /= lp then defaultMeteor {x=x, y=y} {vx=x, vy=0} r clr :: meteors else meteors

removeMeteors : (Int, Int) -> [Meteor] -> [Meteor]
removeMeteors window meteors = filter (\m -> removeMeteor window m) meteors

removeMeteor : (Int, Int) -> Meteor -> Bool
removeMeteor (w, h) {x, y} = y > toFloat -h/2-10

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

-- display
display: (Int, Int) -> Game -> Input -> Element
display (w, h) ({spaceship, bullets, meteors} as game) input =
  collage w h
  [ debug (w,h) game input
  , drawSpaceship spaceship
  , drawBullets bullets
  , drawMeteors meteors
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

debug : (Int, Int) -> Game -> Input -> Form
debug (w, h) game input =
  let x = toFloat w/4
      y = toFloat h/4
  in group
  [ toForm (asText [game.spaceship.x, game.spaceship.y]) |> move (x, y)
  , toForm (asText [input.dx, input.dy]) |> move (x, y+20)
  , toForm (asText [fst input.window, snd input.window]) |> move (x, y+40)
  , toForm (asText (length game.bullets)) |> move (x, y+60)
  , toForm (asText (length game.meteors)) |> move (x, y+80)
  ]

-- signals
delta = inSeconds <~ fps 60
pulse = every (second / 4)
input = sampleOn delta <| Input <~ lift .x Keyboard.arrows
                                 ~ lift .y Keyboard.arrows
                                 ~ Keyboard.space
                                 ~ delta
                                 ~ pulse
                                 ~ Window.dimensions

-- main
gameState : Signal Game
gameState = foldp stepGame defaultGame input

main = display <~ Window.dimensions ~ gameState ~ input
