import Data.Char

data Line = Line (Int,Int) (Int,Int) deriving (Show, Eq)
data Grid = Grid [[Int]] deriving (Show, Eq)

linePoints :: Int -> Int -> [Int]
linePoints y1 y2
  | y1 == y2 = [y1]
  | y1 > y2 = linePoints y2 y1
  | otherwise = y1 : linePoints (y1+1) y2

updateRow :: [[Int]] -> Int -> Int -> [[Int]]
updateRow [] _ _ = []
updateRow (row:rows) x 0 = updateCol row x : rows
updateRow (row:rows) x y = row : updateRow rows x (y-1)

updateCol :: [Int] -> Int -> [Int]
updateCol [] _ = []
updateCol (c:cs) 0 = (c+1) : cs
updateCol (c:cs) x = c : updateCol cs (x-1)

fixedX x = fmap $ (,) x
fixedY y = fmap $ flip (,) y

lineToPoints :: Line -> [(Int,Int)]
lineToPoints (Line (x1,y1) (x2,y2)) =
  if vert then fixedX x1 $ linePoints y1 y2
    else if horz then fixedY y1 $ linePoints x1 x2
    else []
  where vert = x1==x2
        horz = y1==y2


countPoints :: Grid -> Line -> Grid
countPoints (Grid g) l = Grid $ count g (lineToPoints l)
  where count g [] = g
        count g ((x,y):ps) = count (updateRow g x y) ps

solve :: Grid -> Int
solve (Grid g) = length . filter (>= 2) . concat $ g
                                          
empty :: Int -> Grid
empty n = Grid $ empty' n
  where empty' 0 = []
        empty' m = row n : empty' (m-1)
        row 0 = []
        row x = 0 : row (x-1)

parseLine :: String -> Line
parseLine s = let parts = split '-' s
                  p1 = split ',' (parts !! 0) 
                  p2 = split ',' (tail $ parts !! 1) 
                  n1 = read (p1 !! 0)
                  n2 = read (p1 !! 1)
                  n3 = read (p2 !! 0)
                  n4 = read (p2 !! 1) in
                    Line (n1, n2) (n3, n4)
                

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

main = do
  input <- readFile "input"
  let ls = parseLine <$> (lines input)
      allPoints = ((\(Line x _) -> x) <$> ls) ++ ((\(Line _ y) -> y) <$> ls)
      maxX = foldr (max . fst) 0 allPoints
      maxY = foldr (max . snd) 0 allPoints
      dim = (max maxX maxY) + 1
      render = foldl countPoints (empty dim) ls in
        print $ solve render
      

