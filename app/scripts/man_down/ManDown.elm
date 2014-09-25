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
type Game = { man:Man, blocks:[Block], lastPulse:Time, lastBlock:Maybe Block, state:State, count:Int }
type Input = { dx:Int, restart:Bool, window:(Int, Int), pulse:Time, randWidth:Float, randX:Float }

defaultGame : Game
defaultGame = { man=defaultMan, blocks=[], lastPulse=0, lastBlock=Nothing, state=Play, count=0 }

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
  {dx, restart, window, pulse, randWidth, randX }
  ({man, blocks, lastPulse, lastBlock, state, count} as game) =
  let collidedBlock = getCollidedBlock man blocks
  in { game | man <- ((updateManX dx >> updateManY collidedBlock)) man
            , blocks <- ((addBlock randWidth randX (lastPulse /= pulse) window) >> updateBlocks) blocks
            , lastPulse <- pulse
            , state <- isGameOver man (snd window)
            , count <- updateCount lastBlock collidedBlock count
            , lastBlock <- collidedBlock
     }

getCollidedBlock : Man -> [Block] -> Maybe Block
getCollidedBlock {x, y} blocks =
  let collidedBlock = filter (\b -> y > b.y && y - b.y <= 12 && abs (x - b.x) <= (b.width // 2)) blocks
  in if isEmpty collidedBlock then Nothing else Just (head collidedBlock)

updateManX : Int -> Man -> Man
updateManX dx ({x, y, vx, vy} as man) =
  { man |  x <- x + vx
        ,  y <- y + vy
        , vy <- -3
        , vx <- dx*16
  }

updateManY : Maybe Block -> Man -> Man
updateManY block man =
  case block of
    Nothing -> man
    Just b -> { man | vy <- b.vy }

addBlock : Float -> Float -> Bool -> (Int, Int) -> [Block] -> [Block]
addBlock randWidth randX newPulse window blocks =
  let (winWidth, winHeight) = window
      width = round (50 + 200*randWidth)
      x = round ((randX-0.5) * (toFloat winWidth))
  in if newPulse then { x=x, y=-(winHeight // 2), vx=0, vy=0, width=width } :: blocks else blocks

updateBlocks : [Block] -> [Block]
updateBlocks blocks =
  let updateBlock block =
    { block |  y <- block.y + block.vy
            , vy <- 3
    }
  in map updateBlock blocks

updateCount : Maybe Block -> Maybe Block -> Int -> Int
updateCount lastBlock collidedBlock n =
  case (lastBlock, collidedBlock) of
    (Nothing, Just b) -> n + 1
    _ -> n

isGameOver : Position r -> Int -> State
isGameOver {y} winHeight =
  if y < (-winHeight // 2) || y+60 > (winHeight // 2) then Over else Play

-- display
display : (Int, Int) -> Game -> Element
display (w, h) {man, blocks, state, count} =
  collage w h [ drawMan man
              , drawBlocks blocks
              , drawGameOver (w, h) state
              , displayCount (w, h) count
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
drawGameOver (w, h) s = move (0, toFloat h/4) <| toForm <|
  case s of
    Play -> Element.empty
    Over -> plainText "Game Over"

displayCount : (Int, Int) -> Int -> Form
displayCount (w, h) count =
  let w' = toFloat w/2-20
      h' = toFloat h/2-20
  in move (w', h') <| toForm <| asText count

-- signals
dt : Signal Time
dt = fps 60

pulse : Signal Time
pulse = every (1*second)

randWidth : Signal Float
randWidth = Random.float pulse

randX : Signal Float
randX = Random.float pulse

input : Signal Input
input = sampleOn dt <| Input <~ lift .x Keyboard.arrows
                              ~ Keyboard.space
                              ~ Window.dimensions
                              ~ pulse
                              ~ randWidth
                              ~ randX

-- main
gameState : Signal Game
gameState = foldp stepGame defaultGame input

main : Signal Element
main = display <~ Window.dimensions ~ gameState
