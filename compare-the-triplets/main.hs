solve :: [Int] -> [Int] -> [Int]
solve [] [] = [0, 0]
solve [x] [] = [0, 0]
solve [] [y] = [0, 0]
solve [x] [y]
  | x > y = [1, 0]
  | x < y = [0, 1]
  | otherwise = [0, 0]
solve (x : xs) (y : ys) = zipWith (+) (solve [x] [y]) (solve xs ys)

answer :: [Int] -> String
answer xs = unwords $ map show xs

main = interact $ unwords . map show . (\xs -> solve (head xs) (last xs)) . map ((\xs -> map read xs :: [Int]) . words) . lines