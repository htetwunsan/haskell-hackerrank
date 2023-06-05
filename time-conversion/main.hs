import Text.Printf

solve :: String -> String
solve s = printf "%02d:%02d:%02d" hh' mm ss
  where
    hh = read $ take 2 s :: Int
    mm = read $ take 2 $ drop 3 s :: Int
    ss = read $ take 2 $ drop 6 s :: Int
    period = take 2 $ drop 8 s
    hh'
      | period == "AM" =
          if hh == 12
            then 0
            else hh
      | otherwise = if hh == 12 then hh else hh + 12

main :: IO ()
main = interact solve