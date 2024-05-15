module Lambda where

import Data.List (nub, (\\))
import Data.Char (ord, chr)

data Lambda = Var String
            | App Lambda Lambda
            | Abs String Lambda
            | Macro String

instance Show Lambda where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abs x e) = "λ" ++ x ++ "." ++ show e
    show (Macro x) = x

instance Eq Lambda where
    e1 == e2 = eq e1 e2 ([],[],[])
      where
        eq (Var x) (Var y) (env,xb,yb) = elem (x,y) env || not (elem x xb || elem y yb)
        eq (App e1 e2) (App f1 f2) env = eq e1 f1 env && eq e2 f2 env
        eq (Abs x e) (Abs y f) (env,xb,yb) = eq e f ((x,y):env,x:xb,y:yb)
        eq (Macro x) (Macro y) _ = x == y
        eq _ _ _ = False

-- 1.1.
vars :: Lambda -> [String]
vars expr = nub $ aux expr
  where
    aux (Var x) = [x]
    aux (App e1 e2) = aux e1 ++ aux e2
    aux (Abs x e) = x : aux e
    aux (Macro x) = [x]

-- 1.2.
freeVars :: Lambda -> [String]
freeVars expr = nub $ aux expr []
  where
    aux (Var x) bound
      | x `elem` bound = []
      | otherwise = [x]
    aux (App e1 e2) bound = aux e1 bound ++ aux e2 bound
    aux (Abs x e) bound = aux e (x : bound)
    aux (Macro x) _ = [x]

-- 1.3.
newVar :: [String] -> String
newVar l = head $ filter (\x -> not $ elem x l) stringGen
  -- if last (last l) /= 'z'
  --   then init (last l) ++ [Data.Char.chr (Data.Char.ord (last $ last l) + 1)]
  -- else
  --   map (const 'a') $ last l ++ "a"

-- stringGen :: [String]
-- stringGen = "a" : stringGen
-- stringGen =
--   if last s /= 'z'
--     then init s ++ [Data.Char.chr (Data.Char.ord (last $ last l) + 1)]
--   else
--     map (const 'a') s ++ "a"

stringGen :: [String]
stringGen = concatMap gen [1..]
  where
    gen :: Int -> [String]
    gen len = mapM (const ['a'..'z']) [1..len]

-- 1.4.
isNormalForm :: Lambda -> Bool
isNormalForm = undefined

-- 1.5.
reduce :: String -> Lambda -> Lambda -> Lambda
reduce x e1 e2 = undefined

-- 1.6.
normalStep :: Lambda -> Lambda
normalStep = undefined

-- 1.7.
applicativeStep :: Lambda -> Lambda
applicativeStep = undefined

-- 1.8.
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify = undefined

normal :: Lambda -> [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep
