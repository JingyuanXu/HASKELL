module Hasklet4 where

import Control.Monad
import Control.Applicative

-- ** Stack language syntax

-- | Stack programs.
type Prog = [Cmd]

-- | Stack commands.
data Cmd = Push Int
         | IsZero
         | Neg
         | Add
         | If Prog Prog
  deriving (Eq,Show)


-- ** Stack-tracking monad

-- | A stack of booleans and integers.
type Stack = [Either Bool Int]

-- | A monad that maintains a stack as state and may also fail.
--   (A combination of the State and Maybe monads.)
data StackM a = SM (Stack -> Maybe (a,Stack))

-- | Run a computation with the given initial stack.
runWith :: Stack -> StackM a -> Maybe (a,Stack)
runWith s (SM c) = c s

instance Monad StackM where
  return x   = SM (\s -> Just (x,s))
  SM c >>= f = SM (\s -> c s >>= (\(a,t) -> runWith t (f a)))

instance Functor StackM where
  fmap = liftM

instance Applicative StackM where
  pure  = return
  (<*>) = ap


-- ** Stack-tracking monad primitives

-- | Push a boolean value onto the current stack.
pushB :: Bool -> StackM ()
pushB b = SM (\s-> Just ((),Left b:s))

-- | Push an integer value onto the current stack.
pushI :: Int -> StackM ()
pushI i = SM (\s -> Just ((), Right i:s))

-- | Pop a boolean from the current stack and return it.
popB :: StackM Bool
popB = SM $ \s -> case s of
 Left b: ss-> Just (b,ss)
 _ -> Nothing
-- | Pop an integer from the current stack and return it.
popI :: StackM Int
popI = SM $ \s -> case s of
 Right i: ss-> Just (i,ss)
 _ -> Nothing


-- ** Stack language semantics


-- | Monadic semantics of commands.
cmd :: Cmd -> StackM ()
cmd (Push i)= pushI i
cmd IsZero= do i<-popI; pushB (i==0)
cmd Neg =do i<-popI; pushI (-i)
cmd Add = do i<-popI
             j<-popI;
             pushI (i+j)
cmd (If p1 p2)= do b <- popB; if b then prog p1 else prog p2


-- | Monadic semantics of programs.
prog :: Prog -> StackM ()
prog (x:xs)= do cmd x; prog xs
prog []=SM (\s-> Just ((),s))


-- | Run a stack program with an initially empty stack,
--   returning the resulting stack or an error.
--
--   >>> runProg [Push 2, Push 3, Add]
--   Just [Right 5]
--
--   >>> runProg [Push 2, Push 3, Push 3, Neg, Add, IsZero]
--   Just [Left True,Right 2]
--
--   >>> runProg [Push 3, IsZero, If [Push 4] [Push 5], Neg]
--   Just [Right (-5)]
--
--   >>> runProg [Push 2, Add]
--   Nothing
-- 
--   >>> runProg [Push 2, If [Push 4] [Push 5]]
--   Nothing
--
--   >>> runProg [Push 2, Push 3, IsZero, Push 4, Add]
--   Nothing
--
runProg :: Prog -> Maybe Stack
runProg = fmap snd . runWith [] . prog
