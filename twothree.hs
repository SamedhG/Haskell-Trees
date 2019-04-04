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
    | otherwise = exists right a
exists (ThreeNode x y left middle right) a
    | a == x || a == y = True
    | a < x            = exists left a
    | a < y            = exists middle a
    | otherwise        = exists right a


-- Tree data Parent -> Tree
insertHelp :: Ord(a) => (TwoThreeTree a) -> a -> (TwoThreeTree a) -> TwoThreeTree 

-- will only get leaf if tree is empty so can safely ignore parent
insertHelp Leaf a _ = TwoNode a Leaf Leaf

--TwoNode base Cases
insertHelp (TwoNode x Leaf Leaf) a Leaf
    | a <= x    = ThreeNode a x Leaf Leaf
    | otherwise = ThreeNode x a Leaf Leaf
insertHelp (TwoNode x Leaf Leaf) a (TwoNode pX pLeft pRight)
    | a <= pX   = TwoNode pX (insertHelp pLeft a Leaf) pRight 
    | otherwise = TwoNode pX pLeft (insertHelp pRight a Leaf)
insertHelp (TwoNode x Leaf Leaf) a (ThreeNode pX pY pLeft pMiddle pRight)
    | a <= pX   = ThreeNode pX pY (insertHelp pLeft a Leaf) pMiddle pRight 
    | a <= py   = ThreeNode pX pY pLeft (insertHelp pMiddle a Leaf) pRight
    | otherwise = ThreeNode pX pY pLeft pMiddle (insertHelp pRight a Leaf)

-- TwoNode Recursive Cases
insertHelp (TwoNode x left right) a (TwoNode pX pLeft pRight)
    | a <= px && a <= x = TwoNode pX (insertHelp left a pLeft) pRight
    | a <= px           = TwoNode pX (insertHelp right a pLeft) pRight
    | a <= x            = TwoNode pX pLeft (insertHelp left a pRight)
    | otherwise         = TwoNode pX pLeft (insertHelp right a pRight)
insertHelp (TwoNode x left right) a (ThreeNode pX pY pLeft pMiddle pRight)
    | a <= pX && a <= x = ThreeNode pX pY (insertHelp left a pLeft) pMiddle pRight 
    | a <= pX           = ThreeNode pX pY (insertHelp right a pLeft) pMiddle pRight
    | a <= py && a <= x = ThreeNode pX pY pLeft (insertHelp left a pMiddle) pRight
    | a <= py           = ThreeNode pX pY pLeft (insertHelp right a pMiddle) pRight
    | a <= x            = ThreeNode pX pY pLeft pMiddle (insertHelp left a pLeft) 
    | otherwise`        = ThreeNode pX pY pLeft pMiddle (insertHelp left a pRight)

--Three Node Base Cases
insertHelp (ThreeNode x y Leaf Leaf Leaf) a  (TwoNode pX pLeft pRight)
    | a <= x && a <= pX = ThreeNode x pX (TwoNode a Leaf Leaf) (TwoNode y Leaf Leaf) pRight 
    | a <= x            = ThreeNode pX x pLeft (TwoNode a Leaf Leaf) (TwoNode y Leaf Leaf)
    | a <= y && a <= pX = ThreeNode a pX (TwoNode x Leaf Leaf) (TwoNode y Leaf Leaf) pRight
    | a <= y            = ThreeNode pX a pLeft (TwoNode x Leaf Leaf) (TwoNode y Leaf Leaf)
    | a <= pX           = ThreeNode y pX (TwoNode x Leaf Leaf) (TwoNode a Leaf Leaf) pRight
    | otherwise         = ThreeNode pX y pLeft (TwoNode x Leaf Leaf) (TwoNode a Leaf Leaf)
insertHelp (ThreeNode x y left middle right) a  (TwoNode px pLeft pRight)
    | a < x     =  
    | a < y     = 
    | otherwise = 
-- Tree data -> Tree
insert :: Ord(a) => (TwoThreeTree a) a -> Bool 
insert tree a = insertHelp tree a Leaf 







