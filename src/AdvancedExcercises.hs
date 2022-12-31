{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

data Tree a = Empty | Node (Tree a) a (Tree a)

--displayTree :: Tree a -> IO ()
--displayTree Leaf = putStrLn "Leaf"
--displayTree (Node treeLeft x treeRight) = do
    
tupTreeConstruct :: (Integer, Integer) -> Tree (Integer, Integer)
tupTreeConstruct (a,b) = Node (tupTreeConstruct (a+1,b)) (a,b) (tupTreeConstruct (a,b+1))

invTupTree :: Tree (Integer, Integer)
invTupTree = tupTreeConstruct (0,0)

cutTreeFirstHalf :: Integer -> Tree a -> Tree a
cutTreeFirstHalf 0 _ = Empty
cutTreeFirstHalf _ (Empty) = Empty
cutTreeFirstHalf n (Node treeLeft x treeRight) = Node (cutTreeFirstHalf (n-1) treeLeft) x (cutTreeFirstHalf (n-1) treeRight)

cutTreeSecondHalf :: Integer -> Tree a -> [Tree a]
cutTreeSecondHalf 0 tree = [tree]
cutTreeSecondHalf _ (Empty) = [Empty]
cutTreeSecondHalf n (Node treeLeft _ treeRight) = cutTreeSecondHalf (n-1) treeLeft ++ cutTreeSecondHalf (n-1) treeRight

rootNode :: Tree a -> Tree a
rootNode = cutTreeFirstHalf 1

unfoldLayersTree :: Tree a -> [[Tree a]]
unfoldLayersTree tree =  map (map rootNode . flip cutTreeSecondHalf tree) [0..] 

foldTreeLevels :: (b -> a -> b) -> b -> Tree a -> b
foldTreeLevels _ x Empty = x 
foldTreeLevels f x tree = 
    where 
    unfoldedTree = unfoldLayersTree tree
    foldLayer = foldl ()
--foldTreeLevels _ initial Empty = initial
--foldTreeLevels f acc (Node treeLeft x treeRight)
--    = foldl (\acc x -> ) (f acc x)  [treeLeft, treeRight]

--flattenTree :: Tree a -> [a]


--treeIndex :: Tree a -> Integer -> Tree a
--treeIndex Empty _ = Empty


--heightTree 