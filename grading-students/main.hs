round5 :: Int -> Int
round5 x 
    | x >= 38 && y < 3 = x + y
    | otherwise = x
        where y = 5 - (x `mod` 5)


main :: IO ()
main = interact $ unlines . map show . map (\x -> round5 $ read x) . tail . lines
