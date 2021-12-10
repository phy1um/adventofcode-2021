import Data.List

data State = Open | Close deriving(Eq, Show)
data Kind = Round | Square | Curly | Angle deriving(Eq, Show)
data Entry = Entry Kind State deriving(Eq, Show)

score :: [Kind] -> [Entry] -> Int
score stack [] = stackValue (reverse stack)
score stack ((Entry k s):cs)
  | s == Open = score (k:stack) cs
  | s == Close = if match then score succStack cs else 0 -- ignore corrupt
    where match = if length stack > 0 then k == (head stack) else True
          succStack = if length stack > 0 then tail stack else []

remain :: [Kind] -> [Entry] -> [Kind]
remain stack [] = stack
remain stack ((Entry k s):cs)
  | s == Open = remain (k:stack) cs
  | s == Close = if match then remain stack' cs else []
    where match = k == (head stack)
          stack' = if length stack > 0 then tail stack else []

valueOf :: Kind -> Int
valueOf k
  | k == Round = 1
  | k == Square = 2
  | k == Curly = 3
  | k == Angle = 4

stackValue :: [Kind] -> Int
stackValue [] = 0
stackValue (k:ks) = valueOf k + (5 * stackValue ks)
  
parse :: Char -> Entry
parse '(' = Entry Round Open
parse ')' = Entry Round Close
parse '{' = Entry Curly Open
parse '}' = Entry Curly Close
parse '<' = Entry Angle Open
parse '>' = Entry Angle Close
parse '[' = Entry Square Open
parse ']' = Entry Square Close
parse _ = error "unknown character"

testRaw = ["[({(<(())[]>[[{[]{<()<>>",
  "[(()[<>])]({[<{<<[]>>(",
  "{([(<{}[<>[]}>{[]{[(<()>",
  "(((({<>}<{<{<>}{[]{[]{}",
  "[[<[([]))<([[{}[[()]]]",
  "[{[{({}]{}}([{[{{{}}([]",
  "{<[[]]>}<{[{[{[]{()[[[]",
  "[<(<(<(<{}))><([]([]()",
  "<{([([[(<>()){}]>(<<{{",
  "<{([{{}}[<[[[<>{}]]]>[]]" ]

main = do
  input <- readFile "input"
  let ls = lines input
      entries = (fmap . fmap) parse ls
      scores = fmap (score []) entries
      sorted = sort (filter (/= 0) scores) in
    print $ sorted !! (length sorted `div` 2)

