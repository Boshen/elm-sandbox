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
                 , space:Bool
                 }

type Cell = (Int, Int)
type Cells = Set.Set Cell

gridSize = 4

type Game = { state: State
            , cells: Cells
            , lastClick:Bool
            , lastPos:(Int, Int)
            , lastSpace:Bool
            , n:Int
            }

defaultCells : Cells
defaultCells = Set.fromList []

defaultGame : Game
defaultGame = { state=Pause
              , cells=defaultCells
              , lastClick=False
              , lastPos=(0,0)
              , lastSpace=False
              , n=0
              }

-- update
stepGame : GameInput -> Game -> Game
stepGame {pos, click, window, space}
         ({state, cells, lastClick, lastPos, lastSpace, n} as game) =
  case state of
    Play ->
      { game | cells <- updateCells cells
             , lastSpace <- space
             , state <- if space && space /= lastSpace || (length <| Set.toList cells) == 0 then Pause else Play
             , n <- n + 1
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
countNeighbours cell cells = foldl (\c n-> if Set.member c cells then n+1 else n) 0 (around cell)

grid : Cells -> Cells
grid cells = (concatMap around (Set.toList cells)) |> Set.fromList

around : Cell -> [Cell]
around (x, y) = map (\(i, j) -> (x+i, y+j)) neighbours

neighbours : [(Int, Int)]
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
display (w, h) {cells, state, n} {pos, click, space} =
  collage w h [ drawGrid (w, h) cells
              , drawCell (mouseToCell pos (w,h))
              , debug (w, h) pos cells state n
              ]

drawGrid : (Int, Int) -> Cells -> Form
drawGrid (w, h) cells =
  group <| drawCells cells

drawCells : Cells -> [Form]
drawCells cells = map drawCell (Set.toList cells)

drawCell : Cell -> Form
drawCell (i, j) = move (toFloat i*gridSize, toFloat j*gridSize)
                      <| filled black
                      <| square gridSize

debug (w, h) pos cells state n =
  group [
          move (toFloat w/4-40, toFloat h/2-60) <| toForm <| asText n
        , move (toFloat w/4-40, toFloat h/2-40) <| toForm <| asText <| length <| Set.toList cells
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
mouseToCell (x, y) (w, h) = (div (x-(div w 2)) gridSize, div (-y+(div h 2)) gridSize)
