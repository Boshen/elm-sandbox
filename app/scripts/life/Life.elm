module Life where

import Window
import Set
import Mouse
import Keyboard
import Graphics.Input as Input

-- model
data State = Play | Pause

type GameInput = { pos:(Int, Int)
                 , click:Bool
                 , window:(Int, Int)
                 , pulse:Time
                 , space:Bool
                 }

type Cell = (Int, Int)
type Cells = Set.Set Cell

gridSize = 10

type Game = { state: State
            , cells: Cells
            , lastPulse:Time
            , lastClick:Bool
            , lastPos:(Int, Int)
            }

defaultCells : Cells
defaultCells = Set.fromList []

defaultGame : Game
defaultGame = { state=Pause
              , cells=defaultCells
              , lastPulse=0
              , lastClick=False
              , lastPos=(0,0)
              }

-- update
stepGame : GameInput -> Game -> Game
stepGame {pos, click, window, pulse, space}
         ({state, cells, lastPulse, lastClick, lastPos} as game) =
  case state of
    Play ->
      { game | cells <- updateCells pulse lastPulse cells
             , lastPulse <- pulse
             , state <- if space then Pause else Play
      }
    Pause ->
      { game | cells <- if (lastClick /= click || lastPos /= pos) && click then toggleCells pos window cells else cells
             , lastClick <- click
             , lastPos <- pos
             , state <- if space then Play else Pause
      }

updateCells : Time -> Time -> Cells -> Cells
updateCells pulse lastPulse cells =
  if pulse == lastPulse then cells else
   let allCells = grid cells
       deadCells = Set.diff allCells cells
       aliveCells = cells
   in Set.union (Set.filter (shouldLive aliveCells) aliveCells)
                 (Set.filter (shouldRevive aliveCells) deadCells)

shouldLive : Cells -> Cell -> Bool
shouldLive cells cell =
  let neighbours = countNeighbours cell cells
  in neighbours == 2 || neighbours == 3

shouldRevive : Cells -> Cell -> Bool
shouldRevive cells cell =
  countNeighbours cell cells == 3

countNeighbours : Cell -> Cells -> Int
countNeighbours cell cells = Set.foldl (isNeighbour cell) 0 cells

isNeighbour : Cell -> Cell -> Int -> Int
isNeighbour (x1, y1) (x2, y2) i =
  let dx = abs (x1 - x2)
      dy = abs (y1 - y2)
  in i + if dx <= 1 && dy <= 1 && (dx + dy) /= 0 then 1 else 0

grid : Cells -> Cells
grid cells = (concatMap around (Set.toList cells)) |> Set.fromList

around : Cell -> [Cell]
around (x, y) =
  let offset = [-1..1]
      block1D x = map ((,) x) offset
      block2D = concatMap block1D offset
  in map (\(i, j) -> (x+i, y+j)) block2D

toggleCells : (Int, Int) -> (Int, Int) -> Cells -> Cells
toggleCells pos window cells =
  let newCell = mouseToCell pos window
  in if Set.member newCell cells then
        Set.remove newCell cells else
        Set.insert newCell cells

-- display
display : (Int, Int) -> Game -> GameInput -> Element
display (w, h) {cells, state} {pos, space} =
  collage w h [ drawGrid (w, h) cells
              , drawCell blue (mouseToCell pos (w,h))
              , debug (w, h) pos space cells state
              ]

drawGrid : (Int, Int) -> Cells -> Form
drawGrid (w, h) cells =
  group <| drawCells black cells

drawCells : Color -> Cells -> [Form]
drawCells clr cells = map (drawCell clr) (Set.toList cells)

drawCell : Color -> Cell -> Form
drawCell clr (i, j) = move (toFloat i*gridSize, toFloat j*gridSize)
                      <| filled clr
                      <| square gridSize

debug (w, h) pos space cells state =
  group [ move (toFloat w/4, toFloat h/4+0) <| toForm <| asText space
        --, move (toFloat w/4, toFloat h/4+20) <| toForm <| asText cells
        , move (toFloat w/4, toFloat h/4+20) <| toForm <| asText pos
        , move (toFloat w/4, toFloat h/4+40) <| toForm <| asText state
        ]

-- signals
delta = inSeconds <~ fps 60
deltaCells = every <| millisecond
input = sampleOn delta <| GameInput <~ Mouse.position
                                     ~ dropRepeats Mouse.isDown
                                     ~ Window.dimensions
                                     ~ deltaCells
                                     ~ Keyboard.space

-- main
gameState : Signal Game
gameState = foldp stepGame defaultGame input

main : Signal Element
main = display <~ Window.dimensions ~ gameState ~ input

-- helpers
mouseToCell : (Int, Int) -> (Int, Int) -> Cell
mouseToCell (x, y) (w, h) = (div (x-(div w 2)) gridSize, div (-y+(div h 2)) gridSize)
