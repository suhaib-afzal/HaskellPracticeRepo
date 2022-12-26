{-
 -Once you've installed Haskell from http://www.haskell.org/platform/, load the interpreter with the command ghci.
 -
 -You can load (and reload) this file in the interpreter with the command: ":l 2-starting-out.hs"
 -
 -All functions can be implemented in one line using the material covered in http://learnyouahaskell.com/starting-out
 -
 -All indices are zero based.
 -}

-- Find the penultimate element in list l
--init :: [a] -> [a] 
--init = reverse . tail . reverse

penultimate :: [a] -> a
penultimate = last . init 

-- Find the element at index k in list l
-- For example: "findK 2 [0,0,1,0,0,0]" returns 1
safeFindK :: Int -> [a] -> Maybe a
safeFindK _ [] = Nothing
safeFindK x xs 
    | x < 0          = Nothing
    | x <= length xs = Just $ flip (!!) x xs
    | otherwise      = Nothing
 

-- Determine if list l is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = (==) (foldl (flip(:)) [] list) (list)

{-
 - Duplicate the elements in list xs, for example "duplicate [1,2,3]" would give the list [1,1,2,2,3,3]
 - Hint: The "concat [l]" function flattens a list of lists into a single list. 
 - (You can see the function definition by typing ":t concat" into the interpreter. Perhaps try this with other variables and functions)
 -
 - For example: concat [[1,2,3],[3,4,5]] returns [1,2,3,3,4,5]
 -}
duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = [x,x] ++ duplicate xs

{-
 - Imitate the functinality of zip
 - Hint: The function "min x y" returns the lower of values x and y
 - For example "ziplike [1,2,3] ['a', 'b', 'c', 'd']" returns [(1,'a'), (2, 'b'), (3, 'c')]
 -}
ziplike :: [a] -> [b] -> [(a,b)]
ziplike [] _ = []
ziplike _ [] = []
ziplike (x:xs) (y:ys) = [(x,y)] ++ ziplike xs ys

-- Split a list l at element k into a tuple: The first part up to and including k, the second part after k
-- For example "splitAtIndex 2 [1,1,1,2,2,2]" returns ([1,1,1],[2,2,2])


splitAtIndex :: Int -> [a] -> ([a],[a])
splitAtIndex k list
    | k < 0         = ([],list)
    | k < listLength  = ( [ list!!idesc | idesc <- [0..k] ] ,
                          [ list!!iasc | iasc <- [k+1..listLength-1] ] )
    | k >= listLength = (list,[])
    where listLength = length list


-- Drop the element at index k in list l
-- For example "dropK 3 [0,0,0,1,0,0,0]" returns [0,0,0,0,0,0]
dropK :: Int -> [a] -> [a]
dropK k list
    | k < 0           = list
    | k < listLength  = [ list!!ith | ith <- [0..k-1] ++ [k+1 .. listLength-1] ]
    | k >= listLength = list
    where listLength = length list

-- Extracts elements between ith and kth element in list l. Including i, but not k
-- For example, "slice 3 6 [0,0,0,1,2,3,0,0,0]" returns [1,2,3]
-- If i is less than 0 start the slice from 0
-- If k is after the last element include the last element in the slice
-- If both are after return []
-- If both are before return the first element
slice :: Int -> Int -> [a] -> [a]
slice firstInd lastInd list
    | firstInd > lastInd      = reverse $ slice lastInd firstInd list
    
    | firstInd < 0            = slice 0        lastInd list
    | lastInd < 0             = slice firstInd 0       list
    
    | lastInd > listLength    = slice firstInd   listLength list
    | firstInd > listLength   = slice listLength lastInd    list
    
    | firstInd == listLength  = []
    | lastInd == listLength   = [ list!!i | i <- [firstInd..listLength-1] ]
    
    | firstInd == lastInd     = [list !! firstInd]
    
    | otherwise               = [ list!!i | i <- [firstInd..lastInd-1] ]
    
    where listLength = length list

slice' :: Int -> Int -> [a] -> [a]
slice' firstInd lastInd = drop firstInd . take lastInd

slice'' :: Int -> Int -> [a] -> [a]
slice'' firstInd lastInd = drop firstInd . take (lastInd - firstInd)

-- Insert element x in list l at index k
-- For example, "insertElem 2 5 [0,0,0,0,0,0]" returns [0,0,0,0,0,2,0]
insertElem :: a -> Int -> [a] -> [a]
insertElem x k list = slice' 0 k list ++ [x] ++ slice' k listLength list
    where listLength = length list

-- Rotate list l n places left.
-- For example, "rotate 2 [1,2,3,4,5]" gives [3,4,5,1,2]
cycle' :: Int -> [a] -> [a]
cycle' n l = drop n (l ++ take n l)


