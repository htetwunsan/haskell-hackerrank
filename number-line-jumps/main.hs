solve :: [Int] -> String
solve (x1:v1:x2:v2:_)
    | v1 <= v2 = "NO"
    | x1 > x2 = "NO"
    | x1 == x2 = "YES"
    | otherwise = solve [x1 + v1, v1, x2 + v2, v2]

main :: IO ()
main = interact $ solve . map read . words
