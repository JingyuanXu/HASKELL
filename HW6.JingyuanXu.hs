--Jingyuan Xu
--932428597
module HW6 where

import Prelude hiding (and,or,not,pred,succ,fst,snd,either)

import DeBruijn
import Church


-- | Let's pretend Haskell's Int is restricted to Nats.
type Nat = Int


-- | A simple data type with three cases.
data Result = N Nat | B Bool | P Nat Bool
  deriving (Eq,Show)


-- | Compute numeric value from a result.
--   (This is just an arbitrary function.)
value :: Result -> Int
value (N n)     = n * 3
value (B True)  = 1
value (B False) = 0
value (P n b)   = n + if b then 1 else 0

bool:: Bool->Exp
bool True=true
bool False=false



-- | Task 1: convert a Result into a lambda calculus term.
toExp :: Result -> Exp
toExp (N n)=App(abs4 ( App (Ref 2)(Ref 3))) (num n)
toExp(B b)=App(abs4(App(Ref 1)(Ref 3))) (bool b)
toExp(P n b)=App(abs4(App(Ref 0)(Ref 3))) (app2 pair (num n) (bool b))



-- | Task 2: implement the value function as a lambda calculus term.
valueExp :: Exp
valueExp = Abs (app4 case3
	(Abs(app2 mult (Ref 0) three))
	(Abs (app3 if_ (Ref 0) one zero))
    (Abs (app2 add (App fst (Ref 0))
	               (app3 if_ (App snd (Ref 0)) one zero)))
    (Ref 0))

-- | Run your lambda-encoded value function on a lambda-encoded Result.
runValue :: Result -> Exp
runValue r = eval (App valueExp (toExp r))


-- | A function for testing toExp and valueExp. Checks to see if the lambda
--   calculus encoding returns the same number as the given value function.
--
--   >>> test (N 4)
--   True
--
--   >>> test (B True)
--   True
--
--   >>> test (B False)
--   True
--
--   >>> test (P 5 True)
--   True
--
--   >>> test (P 5 False)
--   True
--
test :: Result -> Bool
test r = num (value r) == eval (App valueExp (toExp r))
