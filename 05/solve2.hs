import Data.Char
import qualified Data.Map as M

data Line = Line (Int,Int) (Int,Int) deriving (Show, Eq)
data Grid = Grid (M.Map (Int,Int) Int)

linePoints :: Int -> Int -> [Int]
linePoints y1 y2
  | y1 == y2 = [y1]
  | y1 > y2 = linePoints y2 y1
  | otherwise = y1 : linePoints (y1+1) y2

diagPoints :: (Int,Int) -> (Int, Int) -> [(Int, Int)]
diagPoints (x1,y1) (x2,y2)
  | x1 > x2 = diagPoints (x2,y2) (x1,y2)
  | x1 == x2 = [(x2, y2)]
  | otherwise = (x1,y1) : diagPoints (x1+1,y1+sgn) (x2, y2)
    where sgn = if y2 > y1 then 1 else -1
 
updateRow :: M.Map (Int,Int) Int -> Int -> Int -> M.Map (Int, Int) Int
updateRow m x y = M.insertWith' (+) (x,y) 1 m

fixedX x = fmap $ (,) x
fixedY y = fmap $ flip (,) y

lineToPoints :: Line -> [(Int,Int)]
lineToPoints (Line (x1,y1) (x2,y2)) =
  if vert then fixedX x1 $ linePoints y1 y2
    else if horz then fixedY y1 $ linePoints x1 x2
    else if diag then diagPoints (x1, y1) (x2, y2)
    else []
  where vert = x1==x2
        horz = y1==y2
        slope = abs $ fromIntegral (x2-x1)/ fromIntegral (y2-y1)
        diag = slope == 2 || slope == 1


countPoints :: Grid -> Line -> Grid
countPoints (Grid g) l = Grid $ count g (lineToPoints l)
  where count g [] = g
        count g ((x,y):ps) = count (updateRow g x y) ps

solve :: Grid -> Int
solve (Grid g) = length . filter (>= 2) . M.elems $ g

                                          
empty :: Grid
empty = Grid M.empty

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
      render = foldl countPoints empty ls in
        print $ solve render
      

