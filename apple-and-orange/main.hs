solve :: [Int] -> [Int]
solve (s:t:a:b:m:n:xs) = [apples, oranges]
    where apples = length $ filter (True==) $ map (\x -> a + x >= s && a + x <= t) $ take m xs
          oranges = length $ filter (True==) $ map (\x -> b + x >= s && b + x <= t) $ drop m xs

main :: IO ()
main = interact $ unlines . map show . solve . map read . words
