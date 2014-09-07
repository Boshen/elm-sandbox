module Life where

import Window
import Set
import Mouse
import Graphics.Input as Input

-- model
data State = Play | Pause

type GameInput = { pos:(Int, Int)
                 , pulse:Time
                 , play: Bool
                 }

type Cell = (Int, Int)
type Cells = Set.Set Cell

type Game = { state: State
            , cells: Cells
            , lastPulse:Time
            }

defaultCells : Cells
defaultCells = Set.fromList [(0,0),(1,0),(0,1),(1,1),(-1,-1),(-1,-2),(-2,-1),(-2,-2)]

defaultGame : Game
defaultGame = { state=Pause
              , cells=defaultCells
              , lastPulse=0
              }

play : Input.Input Bool
play = Input.input False

-- update
stepGame : GameInput -> Game -> Game
stepGame {pos, pulse, play} ({state, cells, lastPulse} as game) =
  let state' = if play then Play else Pause
  in case state of
    Play ->
      { game | cells <- updateCells pulse lastPulse cells
             , lastPulse <- pulse
             , state <- state'
      }
    Pause ->
      { game | state <- state' }

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
  3 == countNeighbours cell cells

countNeighbours : Cell -> Cells -> Int
countNeighbours cell cells = Set.foldl (isNeighbour cell) 0 cells

isNeighbour : Cell -> Cell -> Int -> Int
isNeighbour (x1, y1) (x2, y2) i =
  let dx = abs (x1 - x2)
      dy = abs (y1 - y2)
  in i + if dx <= 1 && dy <= 1 && (dx + dy) /= 0 then 1 else 0

grid : Cells -> Cells
grid cells = (concatMap block2D (Set.toList cells)) |> Set.fromList

block1D : Int -> [Int]
block1D i = [i-1, i, i+1]

block2D : Cell -> [Cell]
block2D (x, y) = combinations (block1D x) (block1D y)

-- display
gridSize = 10

display : (Int, Int) -> Game -> GameInput -> Element
display (w, h) {cells, state} {pos} =
  collage w h [ drawGrid (w, h) cells
              , drawCell blue (div ((fst pos)-(div w 2)) gridSize, div (-(snd pos)+(div h 2)) gridSize)
              , displayPlayPauseButton state (w, h)
              ]

displayPlayPauseButton : State -> (Int, Int) -> Form
displayPlayPauseButton state (w, h) =
  move (toFloat w/2-40, toFloat -h/2+20) <| toForm <| case state
  of Play  -> Input.button play.handle False "Pause"
     Pause -> Input.button play.handle True "Play"

drawGrid : (Int, Int) -> Cells -> Form
drawGrid (w, h) cells =
  group <| drawCells black cells

drawCells : Color -> Cells -> [Form]
drawCells clr cells = map (drawCell clr) (Set.toList cells)

drawCell : Color -> Cell -> Form
drawCell clr (i, j) = move (toFloat i*gridSize, toFloat j*gridSize)
                  <| filled clr
                  <| square gridSize

-- signals
delta = inSeconds <~ fps 30
deltaCells = every second
input = sampleOn delta <| GameInput <~ Mouse.position
                                     ~ deltaCells
                                     ~ play.signal

-- main
gameState : Signal Game
gameState = foldp stepGame defaultGame input

main : Signal Element
main = display <~ Window.dimensions ~ gameState ~ input

-- helpers
combinations : [a] -> [b] -> [(a, b)]
combinations a b =
  case a of
    []     -> []
    x :: xs -> map ((,) x) b ++ combinations xs b
