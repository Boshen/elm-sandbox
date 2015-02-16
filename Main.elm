import Html (..)
import Html.Attributes (..)
import List

link : String -> Html
link name =
  a [href (name ++ ".elm")] [text name]

links : List Html
links =
  List.map link
    [ "Dodger"
    , "Fractal"
    , "Life"
    , "Lsystem"
    , "ManDown"
    , "Snake"
    , "Tracer"
    ]

main : Html
main = main' [] links
