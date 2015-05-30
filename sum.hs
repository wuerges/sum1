import Numeric
import System.Environment
import Text.Printf

readNum = map (fst . head . readHex . reverse . take 8) . takeWhile (not . null) . iterate (drop 8) . reverse


carry n = n `div` (0xffffffff+1)
digit n = n `mod` (0xffffffff+1)


sumDigits [] b = b
sumDigits a [] = a
sumDigits (a:as) (b:bs) = (a+b):(sumDigits as bs)

sumCarrys c [] = [c]
sumCarrys c (x:xs) = (digit $ x + c):(sumCarrys (carry $ x + c) xs)

sumNums a b = sumCarrys 0 $ sumDigits a b 

mul1Nums a b = sumCarrys 0 $ map (a*) b


mulNums [] b = []
mulNums (a:as) bs = mul1Nums a bs `sumNums` (0:mulNums as bs)


printCasa :: Integer -> String
printCasa = printf "%08x"

showNum = concat . map printCasa . reverse


main = do (a:b:[]) <- getArgs
          putStrLn $ showNum (readNum a `sumNums` readNum b)
