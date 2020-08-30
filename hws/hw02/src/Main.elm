module Main exposing (..)
import Defns exposing (..)

repeat : Int -> a -> List a
repeat n x = 
    if n <= 0 then
        []
    else
        x :: repeat (n-1) x
invert : List (List a) -> List (List a)
invert lst =
    case lst of
        [] -> []
        x :: xs -> (List.reverse x) :: invert xs
