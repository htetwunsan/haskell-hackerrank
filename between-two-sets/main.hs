solve :: [Int] -> Int
solve (n:_:xs) = length [x | x <- [lcmm .. gcdd], x `mod` lcmm == 0 && gcdd `mod` x == 0]
    where lcmm = foldr lcm 1 $ take n xs
          gcdd = foldr gcd 0 $ drop n xs


main :: IO ()
main = interact $ show . solve . map read . words
