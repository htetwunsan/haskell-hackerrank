import Text.Printf

solve :: [Int] -> [Double]
solve xs = [pos / n, neg / n, zero / n]
  where
    pos = fromIntegral $ length $ filter (> 0) xs
    neg = fromIntegral $ length $ filter (< 0) xs
    zero = fromIntegral $ length $ filter (== 0) xs
    n = fromIntegral $ length xs

showAnswer :: String -> String
showAnswer q = unlines $ map (printf "%.6f") $ solve $ map read $ tail $ words q

main :: IO ()
main = interact showAnswer