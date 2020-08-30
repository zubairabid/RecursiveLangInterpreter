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
occplus : a -> a -> Int
occplus s comp =
    if s == comp then
        1
    else
        0

count_occurrences : a -> List a -> Int
count_occurrences s slist = 
    case slist of
        [] -> 0
        x :: xs -> (occplus s x) + (count_occurrences s xs)
