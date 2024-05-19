module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative

import Lambda
import Binding
import Data.List

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

-- 2.1. / 3.2.
parseLambda :: String -> Lambda
parseLambda str
    | head str == head "\\" = Abs (head $ wordsWhen (=='.') $ tail str) (parseLambda $ head $ tail $ splitWordBy (=='.') $ tail $ tail str)
    | head str == head "(" = App (parseLambda $ head $ splitByThing (==' ') $ tail $ init str) (parseLambda $ last $ splitByThing (==' ') $ tail $ init str)
    | otherwise = Var str

-- words but for other stuff like (==',')
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                    "" -> []
                    s' -> w : wordsWhen p s''
                        where (w, s'') = break p s'

-- breaker for first occurence
splitWordBy :: (Char -> Bool) -> String -> [String]
splitWordBy _ [] = ["", ""]
splitWordBy check str =
    let (firstPart, rest) = break check str
    in case rest of
        [] -> [firstPart, ""]
        (_:secondPart) -> [firstPart, secondPart]

-- breaker for last occurence -- does not work correctly for stuff like (x (y z)) -> ["(x (y", "z))"]
splitLastBy :: (Char -> Bool) -> String -> [String]
splitLastBy check str =
    case findIndices check str of
        [] -> [str, ""]
        indices -> let lastIndex = last indices
                       (firstPart, secondPart) = splitAt lastIndex str
                   in [firstPart, tail secondPart]

splitByThing :: (Char -> Bool) -> String -> [String]
splitByThing check str = aux check str 0 ""
  where
    aux :: (Char -> Bool) -> String -> Int -> String -> [String]
    aux _ [] _ acc = [reverse acc, ""]
    aux check (x:xs) 0 acc
        | check x = [reverse acc, xs]
    aux check (x:xs) n acc
        | x == '(' = aux check xs (n + 1) (x:acc)
        | x == ')' = aux check xs (n - 1) (x:acc)
        | otherwise = aux check xs n (x:acc)

-- 3.3.
parseLine :: String -> Either String Line
parseLine = undefined
