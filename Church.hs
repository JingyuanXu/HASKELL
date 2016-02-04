-- | This module implements Church encodings of Booleans, natural numbers,
--   tuples, and sums using the nameless lambda calculus implemented in
--   DeBruijn.hs.
--
--   It also implements the "Y" fixpoint combinator and demonstrates its
--   use in the implementation of a factorial function.
module Church where

import Prelude hiding (and,or,not,pred,succ,fst,snd,either)

import DeBruijn


--
-- * Syntactic Sugar
--

-- | Build an abstraction that takes two arguments.
abs2 :: Exp -> Exp
abs2 = Abs . Abs

-- | Build an abstraction that takes three arguments.
abs3 :: Exp -> Exp
abs3 = abs2 . Abs

-- | Build an abstraction that takes four arguments.
abs4 :: Exp -> Exp
abs4 = abs3 . Abs

-- | Build an application to apply a function to two arguments.
app2 :: Exp -> Exp -> Exp -> Exp
app2 f w x = App (App f w) x

-- | Build an application to apply a function to three arguments.
app3 :: Exp -> Exp -> Exp -> Exp -> Exp
app3 f w x y = App (app2 f w x) y

-- | Build an application to apply a function to four arguments.
app4 :: Exp -> Exp -> Exp -> Exp -> Exp -> Exp
app4 f w x y z = App (app3 f w x y) z


--
-- * Church Booleans
--

-- | λxy.x
true :: Exp
true = abs2 (Ref 1)

-- | λxy.y
false :: Exp
false = abs2 (Ref 0)

-- | λbte.bte
if_ :: Exp
if_ = abs3 (app2 (Ref 2) (Ref 1) (Ref 0))

-- | λb. if b false true
not :: Exp
not = Abs (app3 if_ (Ref 0) false true)

-- | λpq. if p q p
and :: Exp
and = abs2 (app3 if_ (Ref 1) (Ref 0) (Ref 1))

-- | λpq. if p p q
or :: Exp
or = abs2 (app3 if_ (Ref 1) (Ref 1) (Ref 0))


-- | Church Boolean tests:
--
--   >>> true == eval (app2 and (app2 or false true) (App not false))
--   True
--   
--   >>> false == eval (app2 or (App not true) (app2 and true false))
--   True


--
-- * Church Numerals
--

-- | λfx.x
zero :: Exp
zero = abs2 (Ref 0)

-- | λfx.fx
one :: Exp
one = abs2 (App (Ref 1) (Ref 0))

-- | λfx.f(fx)
two :: Exp
two = abs2 (App (Ref 1) (App (Ref 1) (Ref 0)))

-- | λfx.f(f(fx))
three :: Exp
three = abs2 (App (Ref 1) (App (Ref 1) (App (Ref 1) (Ref 0))))

-- | Build a Church numeral for an arbitrary natural number.
--   
--   >>> map num [0,1,2,3] == [zero,one,two,three]
--   True
--
num :: Int -> Exp
num n = abs2 (help n (Ref 0))
  where help 0 e = e
        help n e = App (Ref 1) (help (n-1) e)

-- | λnfx.f(nfx)
succ :: Exp
succ = abs3 (App (Ref 1) (app2 (Ref 2) (Ref 1) (Ref 0)))

-- | λnmfx.nf(mfx)
add :: Exp
add = abs4 (app2 (Ref 3) (Ref 1) (app2 (Ref 2) (Ref 1) (Ref 0)))

-- | λnmf.n(mf)
mult :: Exp
mult = abs3 (App (Ref 2) (App (Ref 1) (Ref 0)))

-- | λn. n (λx.false) true
isZero :: Exp
isZero = Abs (app2 (Ref 0) (Abs false) true)

-- | λnfx.n (λgh.h(gf)) (λu.x) (λu.u) 
--
--   See: https://en.wikipedia.org/wiki/Church_encoding#Derivation_of_predecessor_function
pred :: Exp
pred = abs3 (app3 (Ref 2) 
                  (abs2 (App (Ref 0) (App (Ref 1) (Ref 3))))
                  (Abs (Ref 1))
                  (Abs (Ref 0)))


-- | Church numeral tests:
--
--   >>> three == eval (app2 add two one)
--   True
--
--   >>> num 15 == eval (app2 mult (app2 add two three) three)
--   True
--
--   >>> num 5 == eval (App pred (num 6))
--   True


--
-- * Fixpoint combinator
--

-- | λf. (λx.f(xx)) (λx.f(xx))
fix :: Exp
fix = Abs (App (Abs (App (Ref 1) (App (Ref 0) (Ref 0))))
               (Abs (App (Ref 1) (App (Ref 0) (Ref 0)))))

-- | fix (λfn. if (isZero n) one (mult n (f (pred n))))
--
--   >>> num 6 == eval (App fac three)
--   True
--
--   >>> num 24 == eval (App fac (num 4))
--   True
--
fac :: Exp
fac = App fix (abs2 (app3 if_ (App isZero (Ref 0))
                              one 
                              (app2 mult (Ref 0) (App (Ref 1) (App pred (Ref 0))))))


--
-- * Church Tuples
--


-- | λxys.sxy
--
--   >>> two == eval (App fst (app2 pair two true))
--   True
--
--   >>> true == eval (App snd (app2 pair two true))
--   True
--
pair :: Exp
pair = abs3 (app2 (Ref 0) (Ref 2) (Ref 1))

-- | λt.t(λxy.x)
fst :: Exp
fst = Abs (App (Ref 0) true)

-- | λt.t(λxy.y)
snd :: Exp
snd = Abs (App (Ref 0) false)


-- | λxyzs.sxyz
--
--   >>> one == eval (App sel13 (app3 tuple3 one two three))
--   True
--
--   >>> two == eval (App sel23 (app3 tuple3 one two three))
--   True
--
--   >>> three == eval (App sel33 (app3 tuple3 one two three))
--   True
--
tuple3 :: Exp
tuple3 = abs4 (app3 (Ref 0) (Ref 3) (Ref 2) (Ref 1))

-- | λt.t(λxyz.x)
sel13 :: Exp
sel13 = Abs (App (Ref 0) (abs3 (Ref 2)))

-- | λt.t(λxyz.y)
sel23 :: Exp
sel23 = Abs (App (Ref 0) (abs3 (Ref 1)))

-- | λt.t(λxyz.z)
sel33 :: Exp
sel33 = Abs (App (Ref 0) (abs3 (Ref 0)))


--
-- * Church Sums
--


-- | λfgu.ufg
--
--   >>> three == eval (app3 either succ not (App inL two))
--   True
--
--   >>> false == eval (app3 either succ not (App inR true))
--   True
--
either :: Exp
either = pair

-- | λxfg.fx
inL :: Exp
inL = abs3 (App (Ref 1) (Ref 2))

-- | λxfg.fx
inR :: Exp
inR = abs3 (App (Ref 0) (Ref 2))


-- | λfghu.ufgh
--
--   >>> three == eval (app4 case3 succ not fst (App in13 two))
--   True
--
--   >>> false == eval (app4 case3 succ not fst (App in23 true))
--   True
--
--   >>> one == eval (app4 case3 succ not fst (App in33 (app2 pair one two)))
--   True
--
case3 :: Exp
case3 = tuple3

-- | λxfgh.fx
in13 :: Exp
in13 = abs4 (App (Ref 2) (Ref 3))

-- | λxfgh.fx
in23 :: Exp
in23 = abs4 (App (Ref 1) (Ref 3))

-- | λxfgh.fx
in33 :: Exp
in33 = abs4 (App (Ref 0) (Ref 3))
