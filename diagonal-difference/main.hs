solve :: [[Int]] -> Int -> [Int]
solve [] i = [0, 0]
solve (xs : xss) i = zipWith (+) [xs !! i, reverse xs !! i] (solve xss k)
  where
    k = i + 1

absDiff :: [Int] -> Int
absDiff xs = abs $ head xs - last xs

answer :: String -> String
answer q = show $ absDiff $ solve (map (\xs -> map read $ words xs :: [Int]) $ tail $ lines q) 0

main :: IO ()
main = interact answer