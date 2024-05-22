module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative

import Lambda
import Binding
import Data.Char (isAlpha)

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

apply (Parser p) = p

instance Applicative Parser where
  af <*> mp =
    do
      f <- af
      f <$> mp
  pure = return

instance Functor Parser where
  fmap f mp =
    do
      f <$> mp

instance Monad Parser where
    mp >>= f = Parser {
        parse = \s -> case parse mp s of
            Nothing -> Nothing
            Just(val, rst) -> parse (f val) rst
    }

    return x = Parser {parse = \s -> Just (x, s)}

instance Alternative Parser where
  empty = failParser
  p1 <|> p2 = Parser $ \s -> case parse p1 s of
                                Nothing -> parse p2 s
                                x -> x

plusParser :: Parser a -> Parser [a]
plusParser p = do
        x <- p
        xs <- starParser p
        return (x:xs)

starParser :: Parser a -> Parser [a]
starParser p = plusParser p <|> return []

failParser :: Parser a
failParser = Parser {parse = \s -> Nothing}

charParser :: Char -> Parser Char
charParser c = Parser {
    parse = \s -> case s of
        [] -> Nothing
        (x:xs) -> if x == c then Just (x, xs) else Nothing
}

predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser {
    parse = \s -> case s of
        [] -> Nothing
        (x:xs) -> if p x then Just (x, xs) else Nothing
}

varParser :: Parser String
varParser = do
    x <- plusParser (predicateParser isAlpha)
    return x

varLambda :: Parser Lambda
varLambda = do
    x <- varParser
    return (Var x)

absLambda :: Parser Lambda
absLambda = do
    charParser '\\'
    var <- varParser
    charParser '.'
    lambda <- totalParser
    return (Abs var lambda)

appLambda :: Parser Lambda
appLambda = do
    charParser '('
    l1 <- totalParser
    plusParser (charParser ' ')
    l2 <- totalParser
    charParser ')'
    return (App l1 l2)

macroLambda :: Parser Lambda
macroLambda = do
    str <- plusParser (predicateParser (\c -> c >= 'A' && c <= 'Z' || c >= '0' && c <= '9'))
    return (Macro str)

totalParser :: Parser Lambda
totalParser = do
    x <- absLambda <|> appLambda <|> macroLambda <|> varLambda
    return x

-- 2.1. / 3.2.
parseLambda :: String -> Lambda
parseLambda s = case apply totalParser s of
  Just(lambda, _) -> lambda

-- monad-less implementation of the parser
-- 2.1. / 3.2.
-- parseLambda :: String -> Lambda
-- parseLambda str
--     | head str == head "\\" = Abs (head $ wordsWhen (=='.') $ tail str) (parseLambda $ head $ tail $ splitWordBy (=='.') $ tail $ tail str)
--     | head str == head "(" = App (parseLambda $ head $ splitByThing (==' ') $ tail $ init str) (parseLambda $ last $ splitByThing (==' ') $ tail $ init str)
--     | head str >= 'A' && head str <= 'Z' || head str >= '0' && head str <= '9' = Macro str
--     | otherwise = Var str

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