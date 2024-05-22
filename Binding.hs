module Binding where

import Lambda

type Context = [(String, Lambda)]

data Line = Eval Lambda 
          | Binding String Lambda deriving (Eq)

instance Show Line where
    show (Eval l) = show l
    show (Binding s l) = s ++ " = " ++ show l

-- 3.1.
simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]
simplifyCtx ctx step lamb = do
    initial <- evalMacros ctx lamb
    Right $ simplify step initial

evalMacros :: Context -> Lambda -> Either String Lambda
evalMacros _ expr@(Var _) = Right expr
evalMacros context (Abs s e) = Abs s <$> evalMacros context e
evalMacros context (App e1 e2) = App <$> evalMacros context e1 <*> evalMacros context e2
evalMacros context expr@(Macro m) = case lookup m context of
    Just e -> evalMacros context e
    Nothing -> Left $ "Macro not found: " ++ m

normalCtx :: Context -> Lambda -> Either String [Lambda]
normalCtx ctx = simplifyCtx ctx normalStep

applicativeCtx :: Context -> Lambda -> Either String [Lambda]
applicativeCtx ctx = simplifyCtx ctx applicativeStep
