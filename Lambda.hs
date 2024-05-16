module Lambda where

import Data.List (nub, (\\))

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
newVar ls = head $ filter (\x -> not $ elem x ls) stringGen
  where
    stringGen :: [String]
    stringGen = concatMap gen [1..]
      where
        gen :: Int -> [String]
        gen len = mapM (const ['a'..'z']) [1..len]

-- 1.4.
isNormalForm :: Lambda -> Bool
isNormalForm (Var _) = True
isNormalForm (Abs _ e) = isNormalForm e
isNormalForm (App (Abs _ _) _) = False
isNormalForm (App e1 e2) = isNormalForm e1 && isNormalForm e2
isNormalForm (Macro _) = True

-- 1.5.
reduce :: String -> Lambda -> Lambda -> Lambda
reduce x e1 e2 = substitute e1
  where
    substitute :: Lambda -> Lambda
    substitute (Var y)
      | x == y = e2
      | otherwise = if y `elem` freeVars e1 then Var y else Var $ newVar $ freeVars e1
    substitute (App e1' e2') = App (substitute e1') (substitute e2')
    substitute (Abs y e)
      | x == y = Abs y e
      | otherwise = if y `notElem` freeVars e then Abs y (substitute e) else Abs (newVar $ freeVars e) (substitute e)
    substitute (Macro m) = Macro m

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
