solve :: [Int] -> Int
solve xs = length $ filter (== maximum xs) xs

main :: IO ()
main = interact $ show . solve . map read . tail . words