module TwoThree
( TwoThreeTree
, makeTwoThree
, insert
, exists
, toList
) where

-- Two Node = data left right
-- Three Node = data data left middle right
data TwoThreeTree a = 
    Leaf 
    | TwoNode a TwoThreeTree TwoThreeTree  
    | ThreeNode a a TwoThreeTree TwoThreeTree TwoThreeTree
    deriving(Eq)

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


-- Tree data Parent -> Tree
insertHelp :: Ord(a) => (TwoThreeTree a) a (TwoThreeTree a) -> bool 

insertHelp Leaf a _ = TwoNode a Leaf Leaf
insertHelp (TwoNode x Leaf Leaf) a _
    | a <= x    = ThreeNode a x Leaf Leaf
    | otherwise = ThreeNode x a Leaf Leaf
insertHelp (TwoNode x left right) a _
    | a <= x    = TwoNode x (insertHelp left a tree) right
    | otherwise = TwoNode x left (insertHelp right a tree)
    where tree  = TwoNode x left right
insertHelp (ThreeNode x y left middle right) a  (TwoNode px pLeft pRight)

-- Tree data -> Tree
insert :: Ord(a) => (TwoThreeTree a) a -> Bool 
insert tree a = insertHelp tree a Leaf 







