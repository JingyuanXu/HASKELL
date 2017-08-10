{-# LANGUAGE TupleSections #-}


module MonadicDSLs where


-- import Prelude hiding (succ,pred,recip)
import Prelude hiding (drop)
import Data.Maybe
-- import Control.Monad.State


----------------------------------------------------------------------
-- The BOX stance
----------------------------------------------------------------------

-- 
-- Safety Seal
--

-- Flight control
--
type Height = Int

rise :: Int -> Height -> Height
rise x h = h+x

drop :: Int -> Height -> Height
drop x h | h>=x      = h-x
         | otherwise = error "Plane Crash"

takeOff :: Height -> Height
takeOff 0 = 50

land :: Height -> Height
land h = h-h  -- Why not "land _ = 0"? --> Laziness drops error

loop :: Height -> Height
loop = rise 100 . drop 150 . rise 50

--Example programs
--
b1 = rise 40 . loop . rise 100 . takeOff $ 0
b2 = rise 40 . loop            . takeOff $ 0


-- Preventing runtime errors using the Maybe type
--
riseM ::Int ->  Height -> Maybe Height
riseM x h = Just (h+x)
-- riseM _ Nothing  = Nothing

dropM :: Int ->  Height -> Maybe Height
dropM x h        | h>=x      = Just (h-x)
                 | otherwise = Nothing
-- dropM _ Nothing              = Nothing

takeOffM ::  Height -> Maybe Height
takeOffM  _ = Just 50
-- takeOffM Nothing  = Nothing

landM ::  Height -> Maybe Height
landM _        = Just 0
-- landM Nothing  = Nothing

--loopM :: Maybe Height -> Maybe Height
--loopM = riseM 100 . dropM 150 . riseM 50


-- Example programs
--
--b3 = riseM 40 . loopM . riseM 100 . takeOffM $ Just 0
--b4 = riseM 40 . loopM             . takeOffM $ Just 0


-- Exploiting the Monad instance of Maybe
--
mrise ::Int -> Height -> Maybe Height
mrise x h = Just (h+x)  --  =  return (h+x)

mdrop :: Int -> Height -> Maybe Height
mdrop x h | h>=x      = Just (h-x)
          | otherwise = Nothing

mtakeOff :: Height -> Maybe Height
mtakeOff _ = Just 50

mland :: Height -> Maybe Height
mland _ = Just 0

mloop :: Height -> Maybe Height
mloop h = mrise 50 h >>= mdrop 150 >>= mrise 100
--mloop h = mrise 100 . mdrop 150 . mrise 50 h

mloop' h = do u <- mrise 50 h
              d <- mdrop 150 u
              mrise 100 d

-- Example programs
--
b5 = Just 0 >>= mtakeOff >>= mrise 100 >>= mloop >>= mrise 40 
b6 = Just 0 >>= mtakeOff               >>= mloop >>= mrise 40 
b5' = Just 0 >>= mtakeOff >>= mrise 100 >>= mloop' >>= mrise 40 
b6' = Just 0 >>= mtakeOff               >>= mloop' >>= mrise 40 
b5r = mrise 40 =<< (mloop =<< (mrise 100 =<< (mtakeOff =<< Just 0)))

{-

:i (=<<)
(=<<) :: Monad m => (a -> m b) -> m a -> m b
    -- Defined in ‘GHC.Base’
infixr 1 =<<

-}


--
-- Collections
--
plot :: Int -> String
plot n = take n (repeat 'X') ++ "\n"

histogram :: [Int] -> String
-- histogram = (>>= plot)
histogram xs = xs >>= plot

hist :: [Int] -> IO ()
hist = putStrLn . histogram

b7 = hist [5,7,3,6]

cartesian :: [a] -> [b] -> [(a,b)]
-- cartesian xs ys = xs >>= (\x->map (\y->(x,y)) ys) 
cartesian xs ys = xs >>= (\x->map (x,) ys) 

cartesian' xs ys = do x <- xs
                      y <- ys
                      return (x,y)


b8 = cartesian [2..5] ['a'..'c']
b8' = cartesian' [2..5] ['a'..'c']



----------------------------------------------------------------------
-- The LABEL stance
----------------------------------------------------------------------


--
-- Tracing
--

data Trace a = T String a
               deriving Show

instance Monad Trace where
  return x = T "" x
  (T t x) >>= f = let T u y = f x 
                   in T (t++u) y

data Exp = N Int
         | Plus Exp Exp
         | Neg Exp
         deriving Show

eval :: Exp -> Int
eval (N i)       = i
eval (Plus e e') = eval e + eval e'
eval (Neg e)     = -(eval e)

l1 = Plus (N 9) (Neg (Plus (N 3) (N 4)))

trace :: Show a => a -> Trace a
trace x = T (show x++";") x

meval1 :: Exp -> Trace Int
meval1 (N i)       = trace i >> return i
meval1 (Plus e e') = meval1 e  >>= (\i->
                     meval1 e' >>= (\j->
                     trace (i+j) >>
                     return (i+j) ))
meval1 (Neg e)     = meval1 e >>= (\i->
                     trace (-i) >>
                     return (-i))

meval2 :: Exp -> Trace Int
meval2 (N i)       = trace i >>= return
meval2 (Plus e e') = meval2 e  >>= (\i->
                     meval2 e' >>= (\j->
                     trace (i+j) >>=
                     return ))
meval2 (Neg e)     = meval2 e >>= (\i->
                     trace (-i) >>=
                     return )

meval3 :: Exp -> Trace Int
meval3 (N i)       = trace i
meval3 (Plus e e') = meval3 e  >>= (\i->
                     meval3 e' >>= (\j->
                     trace (i+j) ))
meval3 (Neg e)     = meval3 e >>= (\i->
                     trace (-i) )

ltrace :: Show a => String -> a -> Trace a
ltrace s x = T (s++" ==> "++show x++"\n") x

pt :: Trace a -> IO ()
pt (T t _) = putStrLn t

meval4 :: Exp -> Trace Int
meval4 e@(N i)        = ltrace (show e) i
meval4 e@(Plus e1 e2) = meval4 e1 >>= (\i->
                        meval4 e2 >>= (\j->
                        ltrace (show e) (i+j) ))
meval4 e@(Neg e1)     = meval4 e1 >>= (\i->
                        ltrace (show e) (-i) )

meval :: Exp -> Trace Int
meval e@(N i)        = ltrace (show e) i
meval e@(Plus e1 e2) = do i <- meval e1
                          j <- meval e2
                          ltrace (show e) (i+j)
meval e@(Neg e1)     = do i <- meval e1
                          ltrace (show e) (-i)




----------------------------------------------------------------------
-- The EFFECTFUL COMPUTATION stance
----------------------------------------------------------------------


--
-- The State monad
--

data State s a = State (s -> (a,s))

instance Monad (State s) where
  return x 	= State (\s->(x,s))
  State c >>= f	= State (\s->let (x,s') = c s
				 State d = f x 
			      in d s')

runState :: State s a -> s -> (a,s)
runState (State f) = f
-- runState (State f) s = f s

runWith = flip runState

exec = runState
execWith = flip exec

onState :: (s -> s) -> State s ()
onState f = State $ \s->((),f s)

fromState :: (s -> a) -> State s a
fromState f = State $ \s->(f s,s)

readState :: State s s
readState = State $ \s->(s,s)

init :: s -> State s ()
init s = State $ const ((),s)


type Counter = Int

evalC :: Counter -> Exp -> (Int,Counter)
evalC n (N i)       = (i,n)
evalC n (Plus e e') = (i+j,o+1)
                      where (i,m) = evalC n e
                            (j,o) = evalC m e'
evalC n (Neg e)     = (-i,m+1)
                      where (i,m) = evalC n e

e1 = evalC 0 l1
e2 = evalC 0 (Plus l1 l1)



type Count a = State Counter a

incCounter :: Count ()
incCounter = State $ \c->((),c+1)
-- incCounter = onState succ


seval :: Exp -> Count Int
seval (N i)       = return i
seval (Plus e e') = do i <- seval e
                       j <- seval e'
                       incCounter
                       return (i+j)
seval (Neg e)     = do i <- seval e
                       incCounter
                       return (-i)


-- nondeterminism
--
choose :: [a -> a] -> [a] -> [a]
choose []     xs = []
choose (f:fs) xs = map f xs ++ choose fs xs
-- choose fs xs = concatMap (flip map xs) fs
interest p x = x*(1+p/100)
spend = flip (-)
keep = interest 5
wallStreet _ = 0

acc = choose [wallStreet,keep] . choose [spend 400,keep] $ [1000]




--
-- The Pic DSL
--


type Point = (Int,Int)

data Pic = Line Point Point 
         | Circle Point Int
         | Pic :+: Pic
         deriving (Eq,Show)

ctr :: Point 
ctr = (3,2)

pic :: Pic 
pic = Line (1,0) (5,3) :+: Circle ctr 4 :+: Circle ctr 5

pic2 = let p = (2,3) in
           Line (1,0) (5,3) :+: Circle p 4 :+: Circle p 5


type Vec = (Int,Int)

moveP :: Vec -> Point -> Point
moveP (dx,dy) (x,y) = (x+dx,y+dy)

move :: Vec -> Pic -> Pic
move v (Line p q)   = Line (moveP v p) (moveP v q)
move v (Circle p r) = Circle (moveP v p) r
move v (p :+: q)    = move v p :+: move v q

ll :: Pic -> Point
ll (Line (x,y) (u,v)) = (min x u,min y v)
ll (Circle (x,y) r)   = (x-r,y-r)
ll (p :+: q)          = (min x u,min y v)
                         where (x,y) = ll p
                               (u,v) = ll q

ur :: Pic -> Point
ur (Line (x,y) (u,v)) = (max x u,max y v)
ur (Circle (x,y) r)   = (x+r,y+r)
ur (p :+: q)          = (max x u,max y v)
                         where (x,y) = ur p
                               (u,v) = ur q

type Rect = (Point,Point)

bbox :: Pic -> Rect
bbox p = (ll p,ur p)

width :: Pic -> Int
width p = r-l 
          where ((l,_),(_,r)) = bbox p

height :: Pic -> Int
height p = u-d 
           where ((_,d),(u,_)) = bbox p


type Name = String
type Dict a = [(Name,a)]

type Picture a = State (Dict Pic) a


def :: Name -> Pic -> Picture ()
def n p = State $ \s->((),(n,p):s)

get :: Name -> Picture (Maybe Pic)
get n = State $ \s->(lookup n s,s)

pic3 = do def "c" (Circle (1,1) 3)
          Just c <- get "c"
          return (c :+: move (2,2) c)

pic3e = do def "c" (Circle (1,1) 3)
           Just c <- get "d"
           return (c :+: move (2,2) c)

draw :: Picture Pic -> Pic
draw = fst . runWith []




data Val = P {unP :: Pic} | L {unL :: Point} | M {unM :: Int}
           deriving (Eq,Show)

 
type Layout a = State (Dict Val) a


set :: Name -> Val -> Layout ()
set n v = State $ \s->((),(n,v):s)

-- setP n p = set n (P p)
setP n = set n . P
setL n = set n . L
setM n = set n . M

ref :: Name -> Layout Val
ref n = State $ \s->(f s,s)
        where f = fromJust . lookup n

pic4 = do setP "c" (Circle (1,1) 3)
          P c <- ref "c"
          return (c :+: move (2,2) c)

-- refP n = do {x <- ref n; return (unP x)}
-- refP n = ref n >>= return . unP
gref f n =  ref n >>= return . f

refP = gref unP
refL = gref unL
refM = gref unM

pic5 = do setL "p" (1,1)
          setM "r" 3
          p <- refL "p"
          r <- refM "r"
          setP "c" (Circle p r)
          c <- refP "c"
          return (c :+: move (r,r) c)


dr :: Layout Pic -> Pic
dr = fst . runWith []


