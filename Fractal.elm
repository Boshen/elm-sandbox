module Fractal where

import Color (..)
import List (..)
import Graphics.Collage (..)
import Time (..)
import Signal (..)

iters = 20
(wx, wy) = (200, 200)
(dx, dy) = (4/toFloat wx, 4/toFloat wy)
defaultGame = { s = [], pos = (0, -2) }

iterate (r, i) (cr, ci) n =
  let r' = r * r - i * i + cr
      i' = 2 * r * i + ci
      z = r' * r' + i' * i'
  in
     if z <= 4 && n < iters then
        iterate (r', i') (cr, ci) (n + 1)
     else
        (n, z)

range start end n =
  let step = (end - start) / toFloat n
  in indexedMap (\i x -> x + step * (toFloat i)) (repeat n start)

log = logBase 10
c n z =
  let mu = (toFloat n) - log (log z) / log 2
      a = sin mu / 2 + 0.5
      b = cos mu / 2 + 0.5
  in rgb (255) (round <| a * 255) 255

cell (x, y) (i, j) =
  let (n, z) = iterate (0, 0) (i, j) 0
      clr = if n == iters then black else c n z
  in
     move (x - wx / 2, y - wy / 2) <| filled clr <| circle 1

stepGame _ ({s, pos} as game) =
   if fst pos == wx then
      game
   else
    let
        i' = fst pos + 1
    in
      { game | s <- s ++ indexedMap (\j' y -> cell (toFloat i', toFloat j') (snd pos, y)) (range -2 2 wy)
             , pos <- (i', -2 + i' * dx)
      }

display {s} = collage wx wy s

input = fps 60

gameState = foldp stepGame defaultGame input

main = display <~ gameState
