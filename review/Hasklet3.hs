module Hasklet3 where
import  GHC.Conc

-- | A list of pairs of elements of type a AND b.
data ListP a b = NilP
               | ConsP a b (ListP a b)
  deriving (Eq,Show)

-- | A list of elements of either type a OR b.
data ListE a b = NilE
               | ConsL a (ListE a b)
               | ConsR b (ListE a b)
  deriving (Eq,Show)

-- | Containers with two different element types that can be mapped over.
class Bifunctor t where
  bimap :: (a -> c) -> (b -> d) -> t a b -> t c d


-- | Test cases for Bifunctor instances.
--   
--   >>> bimap (+1) (>3) (ConsP 1 2 (ConsP 3 4 NilP))
--   ConsP 2 False (ConsP 4 True NilP)
--   
--   >>> bimap (+1) even (ConsL 1 (ConsR 2 (ConsR 3 (ConsL 4 NilE))))
--   ConsL 2 (ConsR True (ConsR False (ConsL 5 NilE)))
--

-- [Bifunctor instances go here.]

instance Bifunctor ListP where
 bimap _ _ NilP= NilP
 bimap f g (ConsP a b p)= ConsP (f a) (g b) (bimap f g p)

instance Bifunctor ListE where
 bimap _ _ NilE=NilE
 bimap f g (ConsL a e)= ConsL (f a) (bimap f g e)
 bimap f g (ConsR b e)= ConsR (g b) (bimap f g e)

-- | Map over the left elements of a bifunctor.
--
--   >>> mapL (+5) (ConsP 1 2 (ConsP 3 4 NilP))
--   ConsP 6 2 (ConsP 8 4 NilP)
--
--   >>> mapL even (ConsL 1 (ConsR 2 (ConsR 3 (ConsL 4 NilE))))
--   ConsL False (ConsR 2 (ConsR 3 (ConsL True NilE)))
--

--mapL :: Bifunctor t => (a -> c) -> t a b -> t c b
mapL f= bimap f id


-- | Map over the right elements of a bifunctor.
--
--   >>> mapR (+5) (ConsP 1 2 (ConsP 3 4 NilP))
--   ConsP 1 7 (ConsP 3 9 NilP)
--   
--   >>> mapR even (ConsL 1 (ConsR 2 (ConsR 3 (ConsL 4 NilE))))
--   ConsL 1 (ConsR True (ConsR False (ConsL 4 NilE)))
--
mapR f =bimap id f


-- | Containers with two different element types that can be folded to
--   a single summary value.
class Bifoldable t where
  bifoldr :: (a -> c -> c) -> (b -> c -> c) -> c -> t a b -> c


-- | Test cases for Bifoldable instances.
--   
--   >>> let addL x (y,z) = (x+y, z)
--   >>> let mulR x (y,z) = (y, x*z)
--   
--   >>> bifoldr addL mulR (0,1) (ConsP 1 2 (ConsP 3 4 NilP))
--   (4,8)
--   
--   >>> bifoldr addL mulR (0,1) (ConsL 1 (ConsR 2 (ConsR 3 (ConsL 4 NilE))))
--   (5,6)
--

-- [Bifoldable instances go here.]
instance Bifoldable ListP where
 bifoldr _ _ c NilP= c
 bifoldr f g c (ConsP a b p)= f a (g b (bifoldr f g c p))

instance Bifoldable ListE where
 bifoldr _ _ c NilE =c
 bifoldr f g c (ConsL a p)= f a (bifoldr f g c p)
 bifoldr f g c (ConsR b p)= g b (bifoldr f g c p)

-- | Fold over the left elements of a bifoldable.
--
--   >>> foldrL (+) 0 (ConsP 2 3 (ConsP 4 5 NilP))
--   6
--
--   >>> foldrL (*) 1 (ConsL 2 (ConsR 3 (ConsR 4 (ConsL 5 NilE))))
--   10
--

foldrL f c = bifoldr f par c


-- | Fold over the right elements of a bifoldable.
--
--   >>> foldrR (+) 0 (ConsP 2 3 (ConsP 4 5 NilP))
--   8
--
--   >>> foldrR (*) 1 (ConsL 2 (ConsR 3 (ConsR 4 (ConsL 5 NilE))))
--   12
--
foldrR f c = bifoldr par f c
