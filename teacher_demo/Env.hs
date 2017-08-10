
module Env where
import Data.Maybe
-- | Variable names.
type Var = String

-- | An environment mapping variables to integers.
type Env = Var -> Maybe Int


-- | An empty environment.
empty :: Env
--empty = \x -> error $ "Not in environment: " ++ x
empty = \_ -> Nothing

-- | Set a variable to a value in the environment.
set :: Var -> Int -> Env -> Env
set x i env = \y -> if x == y then Just i else env y


-- | Lookup the value for a variable in the environment.
--
--   >>> get "x" (set "x" 3 (set "y" 4 empty))
--   3
--
--   >>> get "y" (set "x" 3 (set "y" 4 empty))
--   4
--   
--   >>> get "z" (set "x" 3 (set "y" 4 empty))
--   *** Exception: Not in environment: z
--
get :: Var -> Env ->Maybe Int
get x env = env x
-- get = flip ($)


-- | Remove a variable from the environment.
-- 
--   >>> get "y" (unset "y" (set "x" 3 (set "y" 4 empty)))
--   *** Exception: Not in environment: y
--
--   >>> get "x" (unset "y" (set "x" 3 (set "y" 4 empty)))
--   3
--
unset :: Var -> Env -> Env
unset x env = \y -> if x == y then empty x else env y

-- | Get the value associated with a variable, or return the given
--   default value if the variable is not set.
--
--   >>> getOr "y" 2 (set "x" 3 (set "y" 4 empty))
--   4
--
--   >>> getOr "z" 2 (set "x" 3 (set "y" 4 empty))
--   2
--
type Name=String
getOr :: Name -> Int -> Env -> Int
getOr n i env = fromMaybe i $ get n env

-- get "x" (setAll [("x",3),("y",4)] (set "z" 5 empty))
-- Just 3

setAll :: [(Var,Int)] -> Env -> Env
setAll ((x,i):xs) env= set x i $ setAll xs env
setAll [] env=env

--mapEnv (+10) (set "x" 3 (set "y" 6 empty))
mapEnv :: (Int -> Int) -> Env -> Env
mapEnv f env= \i-> case env i of
 Just i -> Just (f i)
 _ ->Nothing





