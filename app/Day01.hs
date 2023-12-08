module Day01 (day) where

day :: IO ()
day = do
    content <- getContents
    print $ foldr (+) 0 $ map (digitsToNum . digits Start Nothing) (lines' content)

-- print $ map (\s -> (s, (digits Start Nothing s))) (lines' content)

digitsToNum :: Maybe (Int, Int) -> Int
digitsToNum Nothing = 0
digitsToNum (Just (l, r)) = l * 10 + r

digits :: State -> Maybe (Int, Int) -> String -> Maybe (Int, Int)
digits _ pair [] = pair
digits state pair (c : cs) =
    case (next state c) of
        State state' -> digits state' pair cs
        Number n ->
            let
                pair' = case pair of
                    Just (l, _r) -> (l, n)
                    Nothing -> (n, n)
             in
                digits Start (Just pair') cs

lines' :: String -> [String]
lines' [] = []
lines' s =
    let
        (hd, tl) = line s
     in
        hd : lines' tl

line :: String -> (String, String)
line [] = ([], [])
line ('\n' : cs) = ([], cs)
line (c : cs) =
    let
        (this, rest) = line cs
     in
        (c : this, rest)

data State
    = Start
    | O
    | On
    | T
    | Tw
    | Th
    | Thr
    | Thre
    | F
    | Fo
    | Fou
    | Fi
    | Fiv
    | S
    | Si
    | Se
    | Sev
    | Seve
    | E
    | Ei
    | Eig
    | Eigh
    | N
    | Ni
    | Nin

data Next
    = State !State
    | Number !Int

next :: State -> Char -> Next
next O 'n' = State On
next On 'e' = Number 1
next On 'i' = State Ni --
next T 'w' = State Tw
next Tw 'o' = Number 2
next T 'h' = State Th
next Th 'r' = State Thr
next Thr 'e' = State Thre
next Thre 'e' = Number 3
next Thre 'i' = State Ei --
next F 'o' = State Fo
next Fo 'u' = State Fou
next Fo 'n' = State On --
next Fou 'r' = Number 4
next F 'i' = State Fi
next Fi 'v' = State Fiv
next Fiv 'e' = Number 5
next S 'i' = State Si
next Si 'x' = Number 6
next S 'e' = State Se
next Se 'v' = State Sev
next Se 'i' = State Ei --
next Sev 'e' = State Seve
next Seve 'n' = Number 7
next Seve 'i' = State Ei --
next E 'i' = State Ei
next Ei 'g' = State Eig
next Eig 'h' = State Eigh
next Eigh 't' = Number 8
next N 'i' = State Ni
next Ni 'n' = State Nin
next Nin 'e' = Number 9
next Nin 'i' = State Ni --
next _ 'o' = State O
next _ 't' = State T
next _ 'f' = State F
next _ 's' = State S
next _ 'e' = State E
next _ 'n' = State N
next _ '1' = Number 1
next _ '2' = Number 2
next _ '3' = Number 3
next _ '4' = Number 4
next _ '5' = Number 5
next _ '6' = Number 6
next _ '7' = Number 7
next _ '8' = Number 8
next _ '9' = Number 9
next _ _ = State Start
