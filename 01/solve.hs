
calculateIncrease :: [Int] -> Int
calculateIncrease [] = 0
calculateIncrease (x:[]) = 0
calculateIncrease (x:y:ys) = o + calculateIncrease (y:ys)
  where o = if y > x then 1 else 0

groups :: Int -> [a] -> [[a]]
groups n [] = []
groups n (x:xs) = (groups' n [x] xs) : (groups n xs)

groups' :: Int -> [a] -> [a] -> [a]
groups' n c [] = c
groups' n c (x:xs) = if full
                  then c
                  else groups' n (x:c) xs
                    where full = (length c) >= n

processWindow :: Int -> [Int] -> [Int]
processWindow n = fmap sum . filter (\x -> length x >= 2) . groups n

solve :: [Int] -> Int
solve = calculateIncrease . processWindow 3

main = do
  input <- readFile "input"
  let ans = solve (read <$> lines input) in 
    putStrLn $ show ans

