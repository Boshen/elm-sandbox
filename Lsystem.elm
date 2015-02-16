module Lsystem where

import Color (..)
import Graphics.Collage (..)
import Window
import String
import List
import Signal (..)
import Time (..)

port title : String
port title = "Lindenmayer Systems"

len = 5

-- More curve
angle2 = 90
axiom2 = "LFL+F+LFL"
rule2 c =
  if | c == 'L' -> "-RF+LFL+FR-"
     | c == 'R' -> "+LF-RFR-FL+"
     | otherwise -> String.fromChar c


-- hilbert curve
angle3 = 90
axiom3 = "A"
rule3 c =
  if | c == 'A' -> "-BF+AFA+FB-"
     | c == 'B' -> "+AF-BFB-FA+"
     | otherwise -> String.fromChar c

-- dragon curve
angle4 = 90
axiom4 = "FX"
rule4 c =
  if | c == 'X' -> "X+YF+"
     | c == 'Y' -> "-FX-Y"
     | otherwise -> String.fromChar c

(turn, axiom, replace) = (degrees angle2, axiom2, rule2)

step _ s = String.foldl (String.append << replace) "" s

display (w, h) s =
  let (w', h') = (toFloat w, toFloat h)
  in collage w h [move (-w'/2, 0) <| draw s]

draw = String.foldl trace [((0, 0), 0)] >> List.map fst >> traced (solid black)

trace c pos =
  let ((x, y), a) = List.head pos
  in
    if | c == 'F' -> ((x + len * cos a, y + len * sin a), a) :: pos
       | c == '+' -> ((x, y), a + turn) :: List.tail pos
       | c == '-' -> ((x, y), a - turn) :: List.tail pos
       | otherwise -> pos

dt = every <| 2*second

main = display <~ Window.dimensions ~ (foldp step axiom dt)
