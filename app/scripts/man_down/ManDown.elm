module ManDown where

import Keyboard
import Window
import Graphics.Element as Element

port title : String
port title = "Elm - 是男人就下100层"

-- model
data State = Play | Over
type Position r = { r |  x:Int,  y:Int }
type Velocity r = { r | vx:Int, vy:Int }
type Man = Velocity (Position {})
type Block = Position (Velocity { width:Int })
type Game = { man:Man, blocks:[Block], state:State }
type Input = { dx:Int, restart:Bool, winHeight:Int }

defaultGame : Game
defaultGame = { man=defaultMan, blocks=[], state=Play }

defaultMan : Man
defaultMan = { x=0, y=100, vx=0, vy=0 }

-- updates
stepGame : Input -> Game -> Game
stepGame {dx, restart, winHeight} ({man, blocks, state} as game) =
  case state of
    Play ->
      { game | man <- updateMan dx man
             , blocks <- ((addBlock winHeight) . updateBlocks) blocks
             , state <- isOver man winHeight
      }
    Over ->
      if restart then defaultGame else game

updateMan : Int -> Man -> Man
updateMan dx ({x, y, vx, vy} as man) =
  { man |  x <- x + vx
        ,  y <- y + vy
        , vy <- -10
        , vx <- dx*8
  }

addBlock : Int -> [Block] -> [Block]
addBlock winHeight blocks =
  { x=0, y=0, vx=0, vy=0, width=100 } :: blocks

updateBlocks : [Block] -> [Block]
updateBlocks blocks =
  let updateBlock block =
    { block |  y <- block.y + block.vy
            , vy <- 10
    }
  in map updateBlock blocks

isOver : Position r -> Int -> State
isOver {y} winHeight =
  if y < (div -winHeight 2) then Over else Play

-- display
display : (Int, Int) -> Game -> Element
display (w, h) {man, blocks, state} =
  collage w h [ drawMan man
              , drawBlocks blocks
              , drawGameOver state
              ]

drawMan : Position r -> Form
drawMan {x, y} =
  let x' = toFloat x
      y' = toFloat y
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

drawGameOver : State -> Form
drawGameOver s = toForm <|
  case s of
    Play -> Element.empty
    Over -> plainText "Game Over"

-- signals
dt : Signal Time
dt = fps 60

input : Signal Input
input = sampleOn dt <| Input <~ lift .x Keyboard.arrows
                              ~ Keyboard.space
                              ~ lift snd Window.dimensions

-- main
gameState : Signal Game
gameState = foldp stepGame defaultGame input

main : Signal Element
main = display <~ Window.dimensions ~ gameState
