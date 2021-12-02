
data Command = L Int | R Int | D Int | U Int deriving Show
data Position = At Int Int deriving Show

start :: Position
start = At 0 0

step :: Position -> [Command] -> Position
step p [] = p
step (At x y) (c:cs) = step p' cs
  where p' = case c of
              L i -> At (x - i) y
              R i -> At (x + i) y
              D i -> At x (y + i)
              U i -> At x (y - i)

parseCommand :: String -> Command
parseCommand s = case dir of
  "left" -> L v
  "forward" -> R v
  "up" -> U v
  "down" -> D v
  _ -> error dir
  where ws = words s
        dir = ws !! 0
        v = ((read $ ws !! 1)::Int)

main = do
  input <- readFile "input"
  let (At x y) = step start $ parseCommand <$> lines input in
    putStrLn $ show (x*y)

