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
-- λa.λb.(a (b bFalse bTrue) (b bTrue bFalse))
bXor = Abs "a" $ Abs "b" $ App (App (Var "a") (App (App (Var "b") bFalse) bTrue)) (App (App (Var "b") bTrue) bFalse)

-- 4.2. Pair encodings
pair = undefined
first = undefined
second = undefined

-- 4.3. Natural number encodings
n0 = undefined
n1 = undefined
n2 = undefined
nSucc = undefined
nPred = undefined
nAdd = undefined
nSub = undefined
nMult = undefined

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
