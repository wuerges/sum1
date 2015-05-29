import Numeric
import System.Environment

readNum = map (fst . head . readHex . reverse . take 8) . takeWhile (not . null) . iterate (drop 8) . reverse


carry n = n `div` (0xffffffff+1)
digit n = n `mod` (0xffffffff+1)

sumDigits = zipWith (+) 

sumCarrys c [] = [c]
sumCarrys c (x:xs) = (digit $ x + c):(sumCarrys (carry $ x + c) xs)

sumNums a b = sumCarrys 0 $ sumDigits a b 

showNum = concat . map (\x -> showHex  x "") . reverse


main = do (a:b:[]) <- getArgs
          putStrLn $ showNum (readNum a `sumNums` readNum b)
