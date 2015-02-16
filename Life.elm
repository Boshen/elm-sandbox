module Life where

import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Keyboard
import List
import Mouse
import Set
import Signal (..)
import Text (..)
import Time (..)
import Window

port title : String
port title = "Elm - Game of Life"

-- model
type State = Play | Pause

type alias GameInput =
  { pos:(Int, Int)
  , click:Bool
  , window:(Int, Int)
  , space:Bool
  }

type alias Cell = (Int, Int)
type alias Cells = Set.Set Cell

gridSize = 4

type alias Game =
  { state: State
  , cells: Cells
  , lastClick:Bool
  , lastPos:(Int, Int)
  , lastSpace:Bool
  , n:Int
  , path:Cells
  }

defaultGame : Game
defaultGame = { state=Pause
              , cells=Set.empty
              , lastClick=False
              , lastPos=(0,0)
              , lastSpace=False
              , n=0
              , path=Set.empty
              }

-- update
stepGame : GameInput -> Game -> Game
stepGame {pos, click, window, space}
         ({state, cells, lastClick, lastPos, lastSpace, n, path} as game) =
  case state of
    Play ->
      { game | cells <- updateCells cells
             , lastSpace <- space
             , state <- if space && space /= lastSpace || (List.length <| Set.toList cells) == 0 then Pause else Play
             , n <- n + 1
             , path <- Set.union cells path
      }
    Pause ->
      { game | cells <- if (lastClick /= click || lastPos /= pos) && click then toggleCells pos window cells else cells
             , lastClick <- click
             , lastPos <- pos
             , lastSpace <- space
             , state <- if space && space /= lastSpace then Play else Pause
             , n <- 0
      }

updateCells : Cells -> Cells
updateCells cells = Set.filter (shouldLive cells) (grid cells)

shouldLive : Cells -> Cell -> Bool
shouldLive cells cell =
  let n = countNeighbours cell cells
      isAlive = Set.member cell cells
  in (isAlive && (n == 2 || n == 3)) || (not isAlive && n == 3)

countNeighbours : Cell -> Cells -> Int
countNeighbours cell cells = List.foldl (\c n-> if Set.member c cells then n+1 else n) 0 (around cell)

grid : Cells -> Cells
grid cells = (List.concatMap around (Set.toList cells)) |> Set.fromList

around : Cell -> List Cell
around (x, y) = List.map (\(i, j) -> (x+i, y+j)) neighbours

neighbours : List (Int, Int)
neighbours = [ (-1, 1), (0, 1), (1, 1)
             , (-1, 0),         (1, 0)
             , (-1,-1), (0,-1), (1,-1) ]

toggleCells : (Int, Int) -> (Int, Int) -> Cells -> Cells
toggleCells pos window cells =
  let newCell = mouseToCell pos window
  in if Set.member newCell cells then
        Set.remove newCell cells else
        Set.insert newCell cells

-- display
display : (Int, Int) -> Game -> GameInput -> Element
display (w, h) {cells, state, n, path} {pos, click, space} =
  collage w h [
                drawGrid (w, h) green path
              , drawGrid (w, h) black cells
              , drawCell red (mouseToCell pos (w,h))
              , debug (w, h) pos cells state n
              ]

drawGrid : (Int, Int) -> Color -> Cells -> Form
drawGrid (w, h) clr cells =
  group <| drawCells clr cells

drawCells : Color -> Cells -> List Form
drawCells clr cells = List.map (drawCell clr) (Set.toList cells)

drawCell : Color -> Cell -> Form
drawCell clr (i, j) = move (toFloat i*gridSize, toFloat j*gridSize)
                      <| filled clr
                      <| square gridSize

debug (w, h) pos cells state n =
  group [ move (toFloat w/4-40, toFloat h/2-60) <| toForm <| asText n
        , move (toFloat w/4-40, toFloat h/2-40) <| toForm <| asText <| List.length <| Set.toList cells
        ,  move (toFloat w/4-40, toFloat h/2-20) <| toForm <| plainText <|
        case state of
          Play -> "Playing"
          Pause -> "Paused"
        ]

-- signals
delta = inSeconds <~ fps 30
input = sampleOn delta <| GameInput <~ Mouse.position
                                     ~ dropRepeats Mouse.isDown
                                     ~ Window.dimensions
                                     ~ Keyboard.space

-- main
gameState : Signal Game
gameState = foldp stepGame defaultGame input

main : Signal Element
main = display <~ Window.dimensions ~ gameState ~ input

-- helpers
mouseToCell : (Int, Int) -> (Int, Int) -> Cell
mouseToCell (x, y) (w, h) = ((x-(w // 2)) // gridSize, (-y+(h // 2)) // gridSize)
