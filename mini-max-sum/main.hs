import Data.List (sort)

solve :: [Int] -> (Int, Int)
solve xs = (sum $ init sorted, sum $ tail sorted)
  where
    sorted = sort xs

showAnswer :: String -> String
showAnswer q = unwords $ map show [fst ans, snd ans]
  where
    ans = solve $ map read $ words q

main :: IO ()
main = interact showAnswer