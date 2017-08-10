module Hasklet4 where

import Control.Monad


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
pushB = undefined

-- | Push an integer value onto the current stack.
pushI :: Int -> StackM ()
pushI = undefined

-- | Pop a boolean from the current stack and return it.
popB :: StackM Bool
popB = undefined

-- | Pop an integer from the current stack and return it.
popI :: StackM Int
popI = undefined


-- ** Stack language semantics

-- | Monadic semantics of commands.
cmd :: Cmd -> StackM ()
cmd = undefined

-- | Monadic semantics of programs.
prog :: Prog -> StackM ()
prog = undefined


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
