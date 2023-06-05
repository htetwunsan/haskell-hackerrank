solve :: Int -> [String]
solve n = solve' n 1
  where
    solve' :: Int -> Int -> [String]
    solve' n i
      | i <= n = (replicate (n - i) ' ' ++ replicate i '#') : solve' n (i + 1)
      | otherwise = []

main :: IO ()
main = interact $ unlines . solve . read