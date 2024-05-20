module Default where

import Lambda
import Binding

-- Variables (for convenience)
vx = Var "x"
vy = Var "y"
vz = Var "z"
vf = Var "f"
vg = Var "g"
vh = Var "h"
vm = Var "m"
vn = Var "n"

-- Basic combinators
m = Abs "x" $ App vx vx
i = Abs "x" $ vx
k = Abs "x" $ Abs "y" $ vx
ki = Abs "x" $ Abs "y" $ vy
c = Abs "x" $ Abs "y" $ Abs "z" $ App (App vx vz) vy
y = Abs "f" $ App fix fix
  where fix = Abs "x" $ App vf (App vx vx)

-- 4.1. Boolean encodings
-- λx.λy.x
bTrue = Abs "x" $ Abs "y" vx
-- λx.λy.y
bFalse = Abs "x" $ Abs "y" vy
-- λa.λb.((a b) a)
bAnd = Abs "a" $ Abs "b" $ App (App (Var "a") (Var "b")) (Var "a")
-- λa.λb.((a a) b)
bOr = Abs "a" $ Abs "b" $ App (App (Var "a") (Var "a")) (Var "b")
-- λa.((a bFalse) bTrue)
bNot = Abs "a" $ App (App (Var "a") bFalse) bTrue
-- λa.λb.((a ((b bFalse) bTrue)) ((b bTrue) bFalse))
bXor = Abs "a" $ Abs "b" $ App (App (Var "a") (App (App (Var "b") bFalse) bTrue)) (App (App (Var "b") bTrue) bFalse)

-- 4.2. Pair encodings
pair = Abs "x" $ Abs "y" $ Abs "f" $ App (App (Var "f") (Var "x")) (Var "y")
first = Abs "p" $ App (Var "p") bTrue
second = Abs "p" $ App (Var "p") bFalse

-- 4.3. Natural number encodings
-- n0 = λf.λx.x
n0 :: Lambda
n0 = Abs "f" $ Abs "x" $ Var "x"
-- n1 = λf.λx.f x
n1 :: Lambda
n1 = Abs "f" $ Abs "x" $ App (Var "f") (Var "x")
-- n2 = λf.λx.f (f x)
n2 :: Lambda
n2 = Abs "f" $ Abs "x" $ App (Var "f") $ App (Var "f") (Var "x")
-- SUCC = λn.λf.λx.f (n f x)
nSucc :: Lambda
nSucc = Abs "n" $ Abs "f" $ Abs "x" $ App (Var "f") $ App (App (Var "n") (Var "f")) (Var "x")
-- PRED = λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)
nPred :: Lambda
nPred = Abs "n" $ Abs "f" $ Abs "x" $
    App (App (App (Var "n")
        (Abs "g" $ Abs "h" $ App (Var "h") $ App (Var "g") (Var "f")))
        (Abs "u" $ Var "x"))
        (Abs "u" $ Var "u")
-- ADD = λm.λn.λf.λx.m f (n f x)
nAdd :: Lambda
nAdd = Abs "m" $ Abs "n" $ Abs "f" $ Abs "x" $ App (App (Var "m") (Var "f")) $ App (App (Var "n") (Var "f")) (Var "x")
-- SUB = λm.λn.n PRED m
nSub :: Lambda
nSub = Abs "m" $ Abs "n" $ App (App (Var "n") nPred) (Var "m")
-- MULT = λm.λn.λf.m (n f)
nMult :: Lambda
nMult = Abs "m" $ Abs "n" $ Abs "f" $ App (Var "m") (App (Var "n") (Var "f"))

-- Default Context
defaultContext :: Context
defaultContext = 
    [ ("M", m)
    , ("I", i)
    , ("K", k)
    , ("KI", ki)
    , ("C", c)
    , ("Y", y)
    , ("TRUE", bTrue)
    , ("FALSE", bFalse)
    , ("AND", bAnd)
    , ("OR", bOr)
    , ("NOT", bNot)
    , ("XOR", bXor)
    , ("PAIR", pair)
    , ("FST", first)
    , ("SND", second)
    , ("N0", n0)
    , ("N1", n1)
    , ("N2", n2)
    , ("SUCC", nSucc)
    , ("PRED", nPred)
    ,("ADD", nAdd)
    , ("SUB", nSub)
    , ("MULT", nMult)
    ]
