module Day01 (day) where

import Data.Char (ord)

day :: IO ()
day = do
    content <- getContents
    print $ foldr (+) 0 $ map (digitsToNum . digits Nothing) (lines' content)

digitsToNum :: Maybe (Int, Int) -> Int
digitsToNum Nothing = 0
digitsToNum (Just (l, r)) = l * 10 + r

digits :: Maybe (Int, Int) -> String -> Maybe (Int, Int)
digits pair [] = pair
digits pair (c : cs)
    | c >= '0' && c <= '9' =
        let
            n = ord c - ord '0'
            pair' = case pair of
                Just (l, _r) -> (l, n)
                Nothing -> (n, n)
         in
            digits (Just pair') cs
    | otherwise = digits pair cs

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
