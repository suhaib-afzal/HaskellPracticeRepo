import Data.List ( stripPrefix , elemIndex)
import Data.Maybe ( fromMaybe )

mainM :: IO()
mainM = do
    o <- getLine
    let money = readMoney o
    let dols = fst money
    let cents = snd money

    let restList = answerFunc [25, 10, 5, 1] cents
    print (dols : restList)
    return ()

readMoney :: String -> (Int, Int)
readMoney str = (mapTuple read . mapTuple (filter (/= '.')) . splitAt intOfDot) strippedStr
    where 
    intOfDot = (fromMaybe 0 . elemIndex '.') strippedStr
    strippedStr = fromMaybe [] (stripPrefix "$" str)

mapTuple :: (a -> b) -> (a,a) -> (b,b)
mapTuple f (a1,a2) = (f a1, f a2)

remainAndQuo :: Int -> Int -> (Int, Int)
remainAndQuo num divisor = (num `mod` divisor, num `div` divisor)

answerFunc :: [Int] -> Int -> [Int]
answerFunc [] _ = []
answerFunc (divN:rest) val = snd remAndQuoPair : answerFunc rest (fst remAndQuoPair) 
    where
    remAndQuoPair = remainAndQuo val divN

