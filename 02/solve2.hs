
data Command = F Int | D Int | U Int deriving Show
data Position = At Int Int Int deriving Show

start :: Position
start = At 0 0 0

step :: Position -> [Command] -> Position
step p [] = p
step (At x y aim) (c:cs) = step p' cs
  where p' = case c of
              F i -> At (x + i) (y + (aim * i)) aim
              D i -> At x y (aim + i)
              U i -> At x y (aim - i)

parseCommand :: String -> Command
parseCommand s = case dir of
  "forward" -> F v
  "up" -> U v
  "down" -> D v
  _ -> error dir
  where ws = words s
        dir = ws !! 0
        v = ((read $ ws !! 1)::Int)

main = do
  input <- readFile "input"
  let (At x y _) = step start $ parseCommand <$> lines input in
    putStrLn $ show (x*y)

