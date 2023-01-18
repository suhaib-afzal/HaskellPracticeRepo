import System.IO ( hSetBuffering, stdout, BufferMode(NoBuffering) )


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let n = read input_line :: Int
    
    -- hPutStrLn stderr "Debug messages..."
    
    -- Write answer to stdout
    print (sum (uniquePrimeFac n))
    return ()

uniquePrimeFac :: Int -> [Int]
uniquePrimeFac n = uniqueFac n primes

uniqueFac :: Int -> [Int] -> [Int]
uniqueFac _ [] = []
uniqueFac x (p:ps)
    | x <= 1    = []
    | otherwise = replicate ndivs p ++ uniqueFac rems ps
    where 
    (ndivs,rems) = numDivsRems x p
    

numDivsRems :: Int -> Int -> (Int,Int)
numDivsRems x p = (length divList - 1, last divList)
    where divList = takeWhile (>0) $ iterate (division p) x

division :: Int -> Int -> Int
division p x
    | isntDivis p x = 0
    | otherwise     = x `div` p
    


primes :: [Int]
primes = primeshelp [2..]
    where 
    primeshelp :: [Int] -> [Int]
    primeshelp []     = []
    primeshelp (p:ps) = p : primeshelp ( filter (isntDivis p) ps )

isntDivis :: Int -> Int -> Bool
isntDivis p n = n `mod` p /= 0