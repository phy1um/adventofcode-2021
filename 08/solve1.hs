
test :: [String]
test = ["cdfeb", "fcadb", "cdfeb", "cdbaf", "ab"]

solve :: [String] -> Int
solve = length . filter f . fmap length 
  where f x = x == 2 || x == 3 || x == 4 || x == 7

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

main = do
  input <- lines <$> readFile "input"
  let kept = fmap (\x -> (wordsWhen (== '|') x) !! 1) input
      ans = sum $ fmap (solve . words) kept in
      print ans
