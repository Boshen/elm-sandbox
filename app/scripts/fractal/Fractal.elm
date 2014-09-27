module Fractal where

import Window

iters = 20
diameter = 1
init = (0, 0)


iterate : (Float, Float) -> (Float, Float) -> Int -> Int
iterate (r, i) (cr, ci) n =
  let r' = r * r - i * i + cr
      i' = 2 * r * i + ci
      z = r' * r' + i' * i'
  in
    if z <= 4 && n < iters then
      iterate (r', i') (cr, ci) (n + 1)
    else
      n

range : Float -> Float -> Int -> [Float]
range start end n =
  let step = (end - start) / toFloat n
  in indexedMap (\i x -> x + step * (toFloat i)) (repeat n start)

cell : (Float, Float) -> (Float, Float) -> Form
cell (i, j) (dx, dy) =
  let n = iterate init (i, j) 0
      clr = if n == iters then black else rgb (round (toFloat n/iters*255)) 0 0
  in
    move (dx*diameter, dy*diameter) <| filled clr (circle diameter)

draw : (Int, Int) -> [Form]
draw (w, h) =
  let is = range -2 2 (w//3)
      js = range -2 2 (h//3)
  in indexedMap (\dx i ->
                  group <| indexedMap (\dy j ->
                    cell (i, j) (toFloat dx, toFloat dy)
                  ) js
                ) is

grid : (Int, Int) -> Element
grid (w, h) =
  collage w h
  <| map (move (-(toFloat w)/4, -(toFloat h)/4))
  <| draw (w, h)

main = grid <~ Window.dimensions
