{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module AdvancedExcercises where

data Tree a = Empty | Node (Tree a) a (Tree a)

--newtype UnsafeString = ConUnsafeString String

--newtype SafeString = ConSafeString String

--safeStringFromUnsafe :: UnsafeString -> SafeString
--safeStringFromUnsafe (ConUnsafeString unStr) = ConSafeString unStr --DO STUFF HERE

changeSpacesList :: [String] -> [String]
changeSpacesList = reverse 
                   . zipWith (++) (iterate (" " ++) "") 
                   . zipWith (adjustInBetweenSpaces) [1,3,4,8,16,32] 
                   . reverse

adjustInBetweenSpaces :: Int -> String -> String
adjustInBetweenSpaces m = concatMap (adjustInBetweenSpacesHelper m)
    where
    adjustInBetweenSpacesHelper :: Int -> Char -> String
    adjustInBetweenSpacesHelper n ' ' = concat (replicate n " ") --last (take n (iterate (" " ++) " ")) 
    adjustInBetweenSpacesHelper _ c = [c]

powersOfTwo :: [Int]
powersOfTwo = map (2^) [0..]

--removeTempString :: String -> String -> String
--removeTempString tempStr = 

showMaybeList :: Show a => String -> [Maybe a] -> String
showMaybeList tempStr = tail . foldl (\acc x -> acc ++ " " ++ (maybe tempStr show x)) ""

displayTree :: Show a => Tree a -> IO ()
displayTree tree =  mapM_ (putStrLn) unfoldStringTreeNewLines  
    where 
    unfoldStringTreeNewLines = map ("\n " ++) 
                               . changeSpacesList 
                               . map (showMaybeList "T") 
                               $ (unfoldLayersTree tree)

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
cutTreeSecondHalf _ (Empty) = [Empty,Empty]
cutTreeSecondHalf n (Node treeLeft _ treeRight) = cutTreeSecondHalf (n-1) treeLeft ++ cutTreeSecondHalf (n-1) treeRight

rootNodeExtract :: Tree a -> Maybe a
rootNodeExtract Empty = Nothing
rootNodeExtract (Node _ x _) = Just x

maybeOr :: [Maybe a] -> Bool
maybeOr [] = False
maybeOr (Nothing: xs) = maybeOr xs
maybeOr (Just _ : _) = True

-- (map rootNodeExtract . flip cutTreeSecondHalf tree) takes an integer (index)
-- (flip cutTreeSecondHalf tree) will take the integer and return a list of trees that 
-- are rooted at that level in the input tree 
-- (map rootNodeExtract) will then take this list and return the value in the root nodes of these trees
-- TODO: MAKE MORE EFFICIENT
unfoldLayersTree :: Tree a -> [[Maybe a]]
unfoldLayersTree tree = takeWhile (maybeOr) (map (map rootNodeExtract . flip cutTreeSecondHalf tree) [0..])

foldTreeLevels :: (b -> a -> b) -> b -> Tree a -> b
foldTreeLevels _ x Empty = x 
foldTreeLevels f x tree = foldl (f') x flatUnfoldedTree
    where 
    flatUnfoldedTree = (concat . unfoldLayersTree) tree
    f' xb (Nothing) = xb
    f' xb (Just ya) = f xb ya