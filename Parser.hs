module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative

import Lambda
import Binding

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

-- 2.1. / 3.2.
parseLambda :: String -> Lambda
parseLambda str
    | head str == head "\\" = Abs (head $ wordsWhen (=='.') $ tail str) (parseLambda $ head $ tail $ splitWordBy (=='.') $ tail $ tail str)
    | head str == head "(" = App (parseLambda $ head $ splitByThing (==' ') $ tail $ init str) (parseLambda $ last $ splitByThing (==' ') $ tail $ init str)
    | head str >= 'A' && head str <= 'Z' || head str >= '0' && head str <= '9' = Macro str
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
parseLine str =
    case break (== '=') str of
        (var, '=':expr) ->
            if null var || null expr
            then Left "Error: Invalid binding format."
            else Right $ Binding var (parseLambda expr)
        (evalStr, "") ->
            if (head evalStr /= head "\\" && head evalStr /= head "(") && not (null $ last (splitWordBy (== ' ') evalStr))
            then Left "Error: Bad input."
            else Right $ Eval (parseLambda str)