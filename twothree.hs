module TwoThree
( TwoThreeTree
, makeTwoThree
, insert
, exists
, toList
) where

data TwoThreeTree a = 
    Leaf 
    | TwoNode a TwoThreeTree TwoThreeTree  
    | ThreeNode a a TwoThreeTree TwoThreeTree TwoThreeTree

makeTwoThree :: -> TwoThreeTree 
makeTwoThree = Leaf

exists :: Ord(a) => (TwoThreeTree a) a ->  Bool

exists Leaf _ = False
exists (TwoNode x left right) a
    | a == x    = True
    | a < x     = exists left a
    | a > x     = exists right a
    | otherwise = False --Something really bad happened
exists (ThreeNode x y left middle right) a
    | a == x || a == y = True
    | a < x            = exists left a
    | a > y            = exists right a
    | otherwise        = exists middle a


insert :: 
