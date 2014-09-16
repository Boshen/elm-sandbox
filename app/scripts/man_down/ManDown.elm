module ManDown where

import Keyboard
import Window
import Random
import Graphics.Element as Element

port title : String
port title = "Elm - 是男人就下100层"

-- model
data State = Play | Over
type Position r = { r |  x:Int,  y:Int }
type Velocity r = { r | vx:Int, vy:Int }
type Man = Velocity (Position {})
type Block = Position (Velocity { width:Int })
type Game = { man:Man, blocks:[Block], lastPulse:Time, state:State }
type Input = { dx:Int, restart:Bool, winHeight:Int, pulse:Time, rand:Float}

defaultGame : Game
defaultGame = { man=defaultMan, blocks=[], lastPulse=0, state=Play }

defaultMan : Man
defaultMan = { x=0, y=100, vx=0, vy=0 }

-- updates
stepGame : Input -> Game -> Game
stepGame ({restart} as input) ({state} as game) =
  case state of
    Play -> updatePlayState input game
    Over -> if restart then defaultGame else game

updatePlayState : Input -> Game -> Game
updatePlayState
  {dx, restart, winHeight, pulse, rand}
  ({man, blocks, lastPulse, state} as game) =
  let collidedBlock = getCollidedBlock man blocks
  in { game | man <- ((updateManX dx . updateManY collidedBlock)) man
            , blocks <- ((addBlock rand (lastPulse /= pulse) winHeight) . updateBlocks) blocks
            , lastPulse <- pulse
            , state <- isGameOver man winHeight
     }

getCollidedBlock : Man -> [Block] -> Maybe Block
getCollidedBlock {x, y} blocks =
  let collidedBlock = filter (\b -> (y - b.y == 0 || y - b.y ==1) && x >= b.x - (div b.width 2) && x <= b.x + (div b.width 2)) blocks
  in if isEmpty collidedBlock then Nothing else Just (head collidedBlock)

updateManX : Int -> Man -> Man
updateManX dx ({x, y, vx, vy} as man) =
  { man |  x <- x + vx
        ,  y <- y + vy
        , vy <- -1
        , vx <- dx*8
  }

updateManY : Maybe Block -> Man -> Man
updateManY block man =
  case block of
    Nothing -> man
    Just b -> { man | vy <- b.vy }

addBlock : Float -> Bool -> Int -> [Block] -> [Block]
addBlock rand newPulse winHeight blocks =
  let randWidth = round (20 + 100*rand)
  in if newPulse then { x=0, y=-200, vx=0, vy=0, width=randWidth } :: blocks else blocks

updateBlocks : [Block] -> [Block]
updateBlocks blocks =
  let updateBlock block =
    { block |  y <- block.y + block.vy
            , vy <- 1
    }
  in map updateBlock blocks

isGameOver : Position r -> Int -> State
isGameOver {y} winHeight =
  if y < (div -winHeight 2) || y > (div winHeight 2) then Over else Play

-- display
display : (Int, Int) -> Game -> Element
display (w, h) {man, blocks, state} =
  collage w h [ drawMan man
              , drawBlocks blocks
              , drawGameOver (w, h) state
              --, debug (w, h) man blocks
              ]

debug (w, h) man blocks =
  let w' = toFloat w/4
      h' = toFloat h/4
  in group [ move (w', h'+20) <| toForm <| asText man.y
           , move (w', h'+40) <| toForm <| asText <| map .y blocks
           ]

drawMan : Position r -> Form
drawMan {x, y} =
  let x' = toFloat x
      y' = toFloat y+10
      head = moveY 40 <| filled black <| circle 5
      lArm = move (10, 30) <| rotate (degrees 120) <| filled black <| rect 6 20
      rArm = move (-10, 30) <| rotate (degrees 60) <| filled black <| rect 6 20
      body = moveY 22 <| filled black <| rect 10 20
      lLeg = moveX 10 <| rotate (degrees  30) <| filled black <| rect 6 30
      rLeg = moveX -10 <| rotate (degrees 150) <| filled black <| rect 6 30
  in move (x', y') <| group [head, body, lArm, rArm, lLeg, rLeg]

drawBlocks : [Block] -> Form
drawBlocks blocks =
  let drawBlock block = move (toFloat block.x, toFloat block.y) <| filled blue <| rect (toFloat block.width) 10
  in group <| map drawBlock blocks

drawGameOver : (Int, Int) -> State -> Form
drawGameOver (w, h) s = move (toFloat w/4, toFloat h/4) <| toForm <|
  case s of
    Play -> Element.empty
    Over -> plainText "Game Over"

-- signals
dt : Signal Time
dt = fps 60

pulse : Signal Time
pulse = every (4*second)

rand : Signal Float
rand = Random.float pulse

input : Signal Input
input = sampleOn dt <| Input <~ lift .x Keyboard.arrows
                              ~ Keyboard.space
                              ~ lift snd Window.dimensions
                              ~ pulse
                              ~ rand

-- main
gameState : Signal Game
gameState = foldp stepGame defaultGame input

main : Signal Element
main = display <~ Window.dimensions ~ gameState
