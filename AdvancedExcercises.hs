data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)

--displayTree :: Tree a -> IO ()
--displayTree Leaf = putStrLn "Leaf"
--displayTree (Node treeLeft x treeRight) = do
    
tupTreeConstruct :: (Integer, Integer) -> Tree (Integer, Integer)
tupTreeConstruct (a,b) = Node (tupTreeConstruct (a+1,b)) (a,b) (tupTreeConstruct (a,b+1))

invTupTree = tupTreeConstruct (0,0)

cut :: Integer -> Tree a -> Tree a
cut 0 _ = Empty
cut n (Leaf x) = Leaf x
cut n (Node treeLeft x treeRight) = Node (cut (n-1) treeLeft) x (cut (n-1) treeRight)

foldTreeLevels :: (b -> a -> b) -> b -> Tree a -> b
foldTreeLevels _ init Empty = init 
foldTreeLevels f acc (Leaf x) = f acc x
foldTreeLevels f acc (Node treeLeft x treeRight)
    = f acc x 

--flattenTree :: Tree a -> [a]


--treeIndex :: Tree a -> Integer -> Tree a
--treeIndex Empty _ = Empty


--heightTree 