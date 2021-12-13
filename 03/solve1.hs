
invert :: [Bool] -> [Bool]
invert [] = []
invert (True:bs) = False : invert bs
invert (False:bs) = True : invert bs

parseBin :: String -> [Bool]
parseBin [] = []
parseBin ('0':s) = False : parseBin s
parseBin ('1':s) = True : parseBin s

countNth :: [[Bool]] -> Int -> Int
countNth [] n = 0
countNth (x:xs) n = v + (countNth xs n)
  where v = if (x !! n) then 1 else 0

asBin :: [Bool] -> Int
asBin [] = 0
asBin (True:bs) = 1 + 2*(asBin bs)
asBin (False:bs) = 2*(asBin bs)

main = do
  input <- readFile "input"
  let bins = parseBin <$> lines input
      lb = (length $ bins !! 0) - 1
      hw = length bins `div` 2
      res = reverse $ fmap (\x -> if x > hw then True else False) $ fmap (countNth bins) [0 .. lb] in
        putStrLn $ show $ (asBin res) * (asBin $ invert res)


