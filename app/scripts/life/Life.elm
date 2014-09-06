module Life where

import Window
import Set

-- model
type Cell = (Int, Int)
type Cells = Set.Set Cell

defaultCells : Cells
defaultCells = Set.fromList [(0,0),(1,0),(0,1),(1,1),(-1,-1),(-1,-2),(-2,-1),(-2,-2)]

-- update
stepGame : Time -> Cells -> Cells
stepGame _ cells = updateCells cells

updateCells : Cells -> Cells
updateCells cells =
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
display (w, h) cells =
  collage w h [drawCells cells]

drawCells : Cells -> Form
drawCells cells = group <| map drawCell (Set.toList cells)

drawCell : Cell -> Form
drawCell (i, j) = move (toFloat i*50, toFloat j*50)
                  <| filled black
                  <| square 50

-- signals
delta = every second

-- main
main : Signal Element
main = display <~ Window.dimensions ~ (foldp stepGame defaultCells delta)

-- helpers
combinations : [a] -> [b] -> [(a, b)]
combinations a b =
  case a of
    []     -> []
    x :: xs -> map ((,) x) b ++ combinations xs b
