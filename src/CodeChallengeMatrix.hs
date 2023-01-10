{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
import Control.Monad (replicateM)

main :: IO ()
main = do
    nString <- getLine
    let n = read nString :: Int 
    line <- replicateM n $ do 
        nthLine <- getLine
        let nthLineList = map read (words nthLine) :: [Int]
        return nthLineList
    let answer = (trace line) + (minorTrace line)
    print answer
    return ()

lowerTriangle :: (Num a ) => [[a]] -> [[a]]
lowerTriangle [] = []
lowerTriangle matrix = zipWith (take) [1..length(matrix)] matrix

trace :: (Num a) => [[a]] -> a
trace matrix = sum . map (last) $ lowerTriangleOfMat
    where lowerTriangleOfMat = lowerTriangle matrix

minorTrace :: (Num a) => [[a]] -> a
minorTrace matrix = sum . map (last) $ rightLowerTriangleOfMat
    where rightLowerTriangleOfMat = lowerTriangle $ map (reverse) matrix
