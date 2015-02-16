module ManDown where

import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Keyboard
import List
import Random
import Signal (..)
import Text
import Time (..)
import Window

port title : String
port title = "Elm - Man Down"

-- model
type State = Play | Over
type alias Position r = { r |  x:Int,  y:Int }
type alias Velocity r = { r | vx:Int, vy:Int }
type alias Man = Velocity (Position {})
type alias Block = Position (Velocity { width:Int })
type alias Game = { man:Man, blocks:List Block, lastPulse:Time, lastBlock:Maybe Block, state:State, count:Int, seed: Random.Seed }
type alias Input = { dx:Int, restart:Bool, window:(Int, Int), pulse:Time }

defaultGame : Game
defaultGame = { man=defaultMan, blocks=[], lastPulse=0, lastBlock=Nothing, state=Play, count=0, seed=Random.initialSeed 42}

defaultMan : Man
defaultMan = { x=0, y=100, vx=0, vy=0 }

genFloat = Random.generate (Random.float 0 1)

-- updates
stepGame : Input -> Game -> Game
stepGame ({restart} as input) ({state} as game) =
  case state of
    Play -> updatePlayState input game
    Over -> if restart then defaultGame else game

updatePlayState : Input -> Game -> Game
updatePlayState
  {dx, restart, window, pulse}
  ({man, blocks, lastPulse, lastBlock, state, count, seed} as game) =
  let collidedBlock = getCollidedBlock man blocks
  in { game | man <- ((updateManX dx >> updateManY collidedBlock)) man
            , blocks <- ((addBlock seed (lastPulse /= pulse) window) >> updateBlocks) blocks
            , lastPulse <- pulse
            , state <- isGameOver man (snd window)
            , count <- updateCount lastBlock collidedBlock count
            , lastBlock <- collidedBlock
            , seed <- snd <| genFloat seed
     }

getCollidedBlock : Man -> List Block -> Maybe Block
getCollidedBlock {x, y} blocks =
  let collidedBlock = List.filter (\b -> y > b.y && y - b.y <= 12 && abs (x - b.x) <= (b.width // 2)) blocks
  in if List.isEmpty collidedBlock then Nothing else Just (List.head collidedBlock)

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

addBlock : Random.Seed -> Bool -> (Int, Int) -> List Block -> List Block
addBlock seed newPulse window blocks =
  let (winWidth, winHeight) = window
      (randWidth, seed') = genFloat seed
      (randX, _) = genFloat seed'
      width = round (50 + 200*randWidth)
      x = round ((randX-0.5) * (toFloat winWidth))
  in if newPulse then { x=x, y=-(winHeight // 2), vx=0, vy=0, width=width } :: blocks else blocks

updateBlocks : List Block -> List Block
updateBlocks blocks =
  let updateBlock block =
    { block |  y <- block.y + block.vy
            , vy <- 3
    }
  in List.map updateBlock blocks

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

drawBlocks : List Block -> Form
drawBlocks blocks =
  let drawBlock block = move (toFloat block.x, toFloat block.y) <| filled blue <| rect (toFloat block.width) 10
  in group <| List.map drawBlock blocks

drawGameOver : (Int, Int) -> State -> Form
drawGameOver (w, h) s = move (0, toFloat h/4) <| toForm <|
  case s of
    Play -> empty
    Over -> Text.plainText "Game Over"

displayCount : (Int, Int) -> Int -> Form
displayCount (w, h) count =
  let w' = toFloat w/2-20
      h' = toFloat h/2-20
  in move (w', h') <| toForm <| Text.asText count

-- signals
dt : Signal Time
dt = fps 60

pulse : Signal Time
pulse = every (1*second)

input : Signal Input
input = sampleOn dt <| Input <~ map .x Keyboard.arrows
                              ~ Keyboard.space
                              ~ Window.dimensions
                              ~ pulse

-- main
gameState : Signal Game
gameState = foldp stepGame defaultGame input

main : Signal Element
main = display <~ Window.dimensions ~ gameState
