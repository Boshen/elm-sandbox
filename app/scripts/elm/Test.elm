module Test where

import Keyboard
import Window

type Bullet = { x:Float, y:Float, vx: Float, vy: Float, mv:Bool}
bullet : Bullet
bullet = { x=0, y=0, vx=0, vy=0, mv=False}

tank : { x:Float, y:Float, angle:Float, bullet:Bullet}
tank = { x=0, y=0, angle=90, bullet=bullet }

stepTank {x, y} tank =
  let x' = toFloat x
      y' = toFloat y
      angle' = tank.angle + y'
  in { tank | x <- tank.x + x'
            , angle <-  max 0 <| min angle' 180
      }

stepBullet t shoot tank =
  let dx = cos (degrees (180-tank.angle))
      dy = sin (degrees tank.angle) + t*tank.bullet.vy
  in
    { tank | bullet <- if tank.bullet.mv
                       then { bullet | x <- tank.bullet.x + 3*dx
                                     , y <- max 0 tank.bullet.y + 3*dy
                                     , vx <- tank.bullet.vx - t/2
                                     , vy <- tank.bullet.vy - 8*t
                                     , mv <- True
                            }
                       else { bullet | mv <- shoot || tank.bullet.mv
                            }
    }

step (t, arrows, shoot) = stepTank arrows . stepBullet t shoot

delta = inSeconds <~ fps 25

input = sampleOn delta (lift3 (,,) delta Keyboard.arrows Keyboard.space)

display (w, h) {x, y, angle, bullet} =
  let dx = 10*cos(degrees (180-angle))
      dy = 10*sin(degrees (180-angle))
  in collage w h
    [ move (toFloat w/4, toFloat h/4) <| toForm (asText [x, y, angle, bullet.x, bullet.y])
    , move (toFloat w/4, toFloat h/4-30) <| toForm (asText [bullet.vx, bullet.vy])
    , move (x, y) <| filled black (rect 20 5)
    , move (x+dx, y+dy) <| rotate (degrees (90-angle)) <| filled black (rect 2 20)
    , move (bullet.x, bullet.y) <| filled red (circle 3)
    ]

main = lift2 display Window.dimensions (foldp step tank input)
