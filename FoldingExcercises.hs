revr :: [a] -> [a] 
revr = foldr (\x acc -> acc ++ [x]) []

revl :: [a] -> [a]
revl = foldl (\acc x -> x:acc) []

prefixes :: [a] -> [[a]]
prefixes xs = revr $ foldl (\acc x -> (takeFromxs x):acc) [] [1..(lengthxs)]
    where
    takeFromxs x = take x xs
    lengthxs = length xs
    
prefixes' :: [a] -> [[a]]
prefixes' = foldr ( \x acc -> [x] : (map (x:) acc) ) []



basisTerm :: Float -> Float -> Float -> Float
basisTerm jthSelected input listTerm = (input - listTerm) / (jthSelected - listTerm)

removeIndex :: [a] -> Int -> [a]
removeIndex l i = (take i l) ++ (drop (i+1) l)

lagrangePolyHelper :: [Float] -> Float -> Float -> Float
lagrangePolyHelper xdata input jth_xdata 
    =  foldr (\x acc -> x*acc) 1 (map (basisTerm jth_xdata input) xdata)
    
lagrangePoly :: [Float] -> Float -> Int -> Float
lagrangePoly l x i = lagrangePolyHelper (removeIndex l i) x (l !! i) 

lagrangeInterpol :: [(Float, Float)] -> Float -> Float
lagrangeInterpol l z = foldr (\(x,y) acc -> acc + x * y) 0 lagrangexs_and_ys
    where 
    lagrangexs_and_ys = zip (map (lagrangePoly xs z) [0 .. n-1]) ys
    n = length l
    xs = map (fst) l
    ys = map (snd) l


data Trie a = Leaf a | Node a [Trie a]

foldTrie :: (b -> a -> b) -> b -> Trie a -> b
foldTrie f acc (Leaf leafval) 
    = (f acc leafval)
foldTrie f acc (Node nodeval childTries)
    = foldl (\acclist x -> foldTrie f acclist x) (f acc nodeval) childTries


    