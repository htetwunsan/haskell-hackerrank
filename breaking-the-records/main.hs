solveMin :: [Int] -> Int
solveMin (x:xs) = solve [x,0] xs
    where solve :: [Int] -> [Int] -> Int
          solve (mn:c:_) [] = c
          solve (mn:c:_) (y:ys) = solve [mnn, cc] ys
              where mnn = min mn y
                    cc = if y < mn then c + 1 else c

solveMax :: [Int] -> Int
solveMax (x:xs) = solve [x,0] xs
    where solve :: [Int] -> [Int] -> Int
          solve (mx:c:_) [] = c
          solve (mx:c:_) (y:ys) = solve [mxx, cc] ys
              where mxx = max mx y
                    cc = if y > mx then c + 1 else c

solve :: [Int] -> [Int]
solve xs = [solveMax xs, solveMin xs]

main :: IO ()
main = interact $ unwords . map show . solve . map read . tail . words
