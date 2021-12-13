
data State = Open | Close deriving(Eq, Show)
data Kind = Round | Square | Curly | Angle deriving(Eq, Show)
data Entry = Entry Kind State deriving(Eq, Show)

score :: [Kind] -> [Entry] -> Int
score [] [] = 0
score stack [] = 0 -- incomplete
score stack ((Entry k s):cs)
  | s == Open = score (k:stack) cs
  | s == Close = if match then score succStack cs else valueOf k
    where match = k == (head stack)
          succStack = if length stack > 0 then tail stack else []

valueOf :: Kind -> Int
valueOf k
  | k == Round = 3
  | k == Square = 57
  | k == Curly = 1197
  | k == Angle = 25137
  
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
      entries = (fmap . fmap) parse ls in
    print $ sum $ fmap (score []) entries

