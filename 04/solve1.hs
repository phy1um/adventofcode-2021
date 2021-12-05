
data Cell = Cell Int Bool deriving (Eq, Show)

cell :: Int -> Cell
cell n = Cell n False

type Row = [Cell]
type Card = [Row]

call :: [Card] -> Int -> [Card]
call [] _ = []
call (c:cs) n = c' : call cs n
  where c' = (fmap . fmap) markMatch c
        markMatch (Cell i b) = if n == i
                                then (Cell i True)
                                else (Cell i b)

get :: Int -> Int -> [[a]] -> a
get x y t = t !! x !! y

check :: Card -> Bool
check rs = rowFound || colFound || diagFound
  where rowFound = foldr (||) False (fmap rowFound' rs)
        rowFound' r = foldr (&&) True (fmap (\(Cell _ b) -> b) r)
        colFound = foldr (||) False [checkCol 0, checkCol 1, checkCol 2, checkCol 3, checkCol 4]
        checkCol n = checkCol' n rs
        checkCol' n [] = True
        checkCol' n (r:rs) = let (Cell _ b) = r !! n in b && checkCol' n rs
        diagFound = False
        --bt = (fmap . fmap) (\(Cell _ b) -> b) rs
        --diag1 = get 0 0 bt && get 1 1 bt && get 2 2 bt && get 3 3 bt && get 4 4 bt
        --diag2 = get 0 4 bt && get 1 3 bt && get 2 2 bt && get 3 1 bt && get 4 0 bt
  
callList :: [Card] -> [Int] -> [Card]
callList cs [] = cs
callList cs (x:xs) = call (callList cs xs) x

getWinner :: [Card] -> [Int] -> (Card, Int)
getWinner cs [] = error "no winner"
getWinner cs (x:xs) = if won then (winner, x) else getWinner succ xs
  where succ = call cs x
        won = foldr (||) False (fmap check succ)
        winner = winner' succ
        winner' (x:xs) = if check x then x else winner' xs
        winner' [] = error "checking for winner that does not exist"


wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

testCard :: Card
testCard = [
  [(cell 1), (cell 2), (cell 3), (cell 4), (cell 5)],
  [(cell 6), (cell 7), (cell 8), (cell 9), (cell 10)],
  [(cell 11), (cell 12), (cell 13), (cell 14), (cell 15)],
  [(cell 16), (cell 17), (cell 18), (cell 19), (cell 20)],
  [(cell 21), (cell 22), (cell 23), (cell 24), (cell 25)]]

makeCards :: [Int] -> [Card]
makeCards [] = []
makeCards xs = [r1, r2, r3, r4, r5] : makeCards (drop 25 xs)
  where r1 = fmap cell (take 5 xs)
        r2 = fmap cell (take 5 (drop 5 xs))
        r3 = fmap cell (take 5 (drop 10 xs))
        r4 = fmap cell (take 5 (drop 15 xs))
        r5 = fmap cell (take 5 (drop 20 xs))

solve = do
  input <- readFile "input"
  let ln = lines input
      seq = head ln
      callNumbers = fmap read (wordsWhen (== ',') seq)
      rawCards = tail ln
      cards = makeCards (fmap read (foldr (++) [] (words <$> rawCards))) in
        let (c, i) = getWinner cards callNumbers
            unmarked = filter (\(Cell _ b) -> not b) (concat c)
            asInts = fmap (\(Cell i _) -> i) unmarked in
              print $ (sum asInts) * i

        
    
  
