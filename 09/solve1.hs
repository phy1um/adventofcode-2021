import Data.Char

data Grid = Grid [Int] Int Int

get :: Grid -> (Int, Int) -> Int
get (Grid hs width _) (x, y) = hs !! index
  where index = (y * width) + x

bounds :: Int -> Int -> (Int, Int) -> Bool
bounds w h (x, y) = if x < 0 || y < 0 || x >= w || y >= h
                                then False else True

neighbors :: Grid -> (Int, Int) -> [(Int, Int)]
neighbors (Grid _ w h) (x,y) = filter (bounds w h) [n1, n2, n3, n4]
  where n1 = (x-1, y)
        n2 = (x+1, y)
        n3 = (x, y-1)
        n4 = (x, y+1)

risk :: Grid -> (Int, Int) -> Int
risk (Grid ps w h) (x,y) = if low then value + 1 else 0
  where value = get (Grid ps w h) (x,y)
        ns = neighbors (Grid ps w h) (x,y)
        nvalues = get (Grid ps w h) <$> ns
        low = foldr (&&) True $ fmap (\x -> value < x) nvalues

test :: String
test = "21999432103987894921985678989287678967899899965678"

allPoints :: Int -> Int -> [(Int, Int)]
allPoints x 0 = (,) 0 <$> [0 .. x]
allPoints x y = ((,) y <$> [0 .. x]) ++ rest
  where rest = allPoints x (y-1)


main = do
  input <- readFile "input"
  let d = digitToInt <$> (filter (/= '\n') input)
      ls = lines input
      width = length (ls !! 0)
      height = length ls
      ps = allPoints (width - 1) (height - 1) in
    print $ sum $ fmap (risk (Grid d width height)) ps
  

