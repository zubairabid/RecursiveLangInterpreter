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
product : List a -> List a -> List (List a)
product sos1 sos2 =
    if List.isEmpty sos1 then
        [sos2]
    else if List.isEmpty sos2 then
        [sos1]
    else
        if (List.length sos1) > 1 then
            case sos1 of
                [] -> [] -- this case should not happen
                x :: xs -> List.append (product [x] sos2) (product xs sos2)
        else if (List.length sos2) > 1 then
            case sos2 of
                [] -> [] -- again, won't happen
                y :: ys -> List.append (product sos1 [y]) (product sos1 ys)
        else
            [List.append sos1 sos2]
every : (a -> Bool) -> List a -> Bool
every pred lst =
    case lst of
        [] -> True
        x :: xs -> (pred x) && (every pred xs)
merge : List Int -> List Int -> List Int
merge loi1 loi2 =
    case loi1 of
        [] -> loi2
        x :: xs ->
            case loi2 of
                [] -> loi1
                y :: ys ->
                    if x < y then
                        x :: (merge xs loi2)
                    else
                        y :: (merge loi1 ys)
preorder : Tree -> List Int
preorder tree =
    case tree of
        Leaf val -> [val]
        Node val left right ->
            List.append [val] (List.append (preorder left) (preorder right))
inorder : Tree -> List Int
inorder tree =
    case tree of
        Leaf val -> [val]
        Node val left right ->
            List.append (inorder left) (List.append [val] (inorder right))
postorder : Tree -> List Int
postorder tree =
    case tree of
        Leaf val -> [val]
        Node val left right ->
            List.append (postorder left) (List.append (postorder right) [val])
count_nodes : Tree -> Int
count_nodes tree =
    case tree of
        Leaf val -> 1
        Node val left right ->
            1 + (count_nodes left) + (count_nodes right)
count_leaves : Tree -> Int
count_leaves tree =
    case tree of
        Leaf val -> 1
        Node val left right ->
            (count_leaves left) + (count_leaves right)
count_internal : Tree -> Int
count_internal tree =
    case tree of
        Leaf val -> 0
        Node val left right ->
            1 + (count_internal left) + (count_internal right)
tree_map : (Int -> Int) -> Tree -> Tree
tree_map fn tr =
    case tr of
        Leaf val -> Leaf (fn val)
        Node val left right ->
            Node (fn val) (tree_map fn left) (tree_map fn right)
value_at_path : List PathItem -> Tree -> Maybe Int
value_at_path path tree = 
    case path of
        [] -> -- the current node is the value
            case tree of
                Leaf v -> Just v
                Node v left right -> Just v
        current :: rest ->
            case current of
                Left ->
                    case tree of
                        Leaf v -> Nothing
                        Node v left right ->
                            value_at_path rest left
                Right ->
                    case tree of
                        Leaf v -> Nothing
                        Node v left right ->
                            value_at_path rest right
