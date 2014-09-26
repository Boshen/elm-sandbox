module Fractal where

import Window
import String

port title : String
port title = "Lindenmayer Systems"

len = 5

-- More curve
angle2 = 90
axiom2 = "LFL+F+LFL"
rule2 c =
  if | c == 'L' -> "-RF+LFL+FR-"
     | c == 'R' -> "+LF-RFR-FL+"
     | otherwise -> show c


-- hilbert curve
angle3 = 90
axiom3 = "A"
rule3 c =
  if | c == 'A' -> "-BF+AFA+FB-"
     | c == 'B' -> "+AF-BFB-FA+"
     | otherwise -> show c

-- dragon curve
angle4 = 90
axiom4 = "FX"
rule4 c =
  if | c == 'X' -> "X+YF+"
     | c == 'Y' -> "-FX-Y"
     | otherwise -> show c

(turn, axiom, replace) = (degrees angle2, axiom2, rule2)

step _ s = String.foldl (String.append << replace) "" s

display (w, h) s =
  let (w', h') = (toFloat w, toFloat h)
  in collage w h [move (-w'/2, 0) <| draw s]

draw = String.foldl trace [((0, 0), 0)] >> map fst >> traced (solid black)

trace c pos =
  let ((x, y), a) = head pos
  in
    if | c == 'F' -> ((x + len * cos a, y + len * sin a), a) :: pos
       | c == '+' -> ((x, y), a + turn) :: tail pos
       | c == '-' -> ((x, y), a - turn) :: tail pos
       | otherwise -> pos

dt = keepIf ((>) 6) 0 <| count <| every <| 2*second

main = display <~ Window.dimensions ~ (foldp step axiom dt)
