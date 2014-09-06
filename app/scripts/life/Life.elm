module Life where

import Window

defaultGame = { cells=[[True, False], [False, True]] }

stepGame t game = game

delta = inSeconds <~ fps 30

display (w, h) {cells} =
  collage w h [drawCells cells]

--drawCells : Array.Array [[Bool]] -> Form
drawCells cells = group <| map drawRow (withIndex cells)

--drawRow : [Int, [Bool]] ->
drawRow (i,row) = group <| map (\(j, b) -> drawCell (i,j) b) (withIndex row)

drawCell (i, j) b = move (toFloat i, toFloat j) <| asText b
--drawCell (i,j) = [toForm [toText j]]

main : Signal Element
main = display <~ Window.dimensions ~ (foldp stepGame defaultGame delta)

withIndex : [a] -> [(Int, a)]
withIndex arr = zip [0..length arr] arr
