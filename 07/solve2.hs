
data Crab = Crab Int deriving(Eq, Show)

test :: [Crab]

test = Crab <$> [16,1,2,0,4,2,7,1,2,14]

f :: Int -> Int
f n = n * (n+1) `div` 2

fuelTo :: [Crab] -> Int -> Int
fuelTo ((Crab i):cs) n = f (abs $ n - i) + (fuelTo cs n)
fuelTo [] _ = 0

-- assume biggest crab position is biggest N

maxCrab :: [Crab] -> Int
maxCrab = (foldr max 0) . (fmap (\(Crab i) -> i))

solve :: [Crab] -> Int -> Int
solve cs n = foldr min 99999999999999 $ fmap (\i -> fuelTo cs i) [0 .. n] 

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

main = do
  input <- readFile "input"
  let crabs = (Crab . read) <$> wordsWhen (== ',') input in
    print $ solve crabs (maxCrab crabs)

