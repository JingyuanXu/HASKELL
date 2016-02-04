--Jingyuan Xu Onid: 932428597
module HW2 where


-- | One step in a path, indicating whether to follow the left subtree (L)
--   or the right subtree (R).
data Step = L | R
  --       |T
  deriving (Eq,Show)


-- | A path is just a sequence of steps. Each node in a binary tree can be
--   identified by a different path, indicating how to walk down the tree
--   starting from the root. See the examples for `valueAt`.
type Path = [Step]


-- | Binary trees with nodes labeled by an arbitrary type.
data Tree a = Node a (Tree a) (Tree a)
            | End
  deriving (Eq,Show)


-- | Create a leaf node.
leaf :: a -> Tree a
leaf x = Node x End End


-- | An example tree.
ex :: Tree Int
ex = Node 4 (Node 3 (leaf 2) End)
            (Node 7 (Node 5 End (leaf 6))
                    (leaf 8))

-- | Get the value at the node specified by a path.
--
--   >>> valueAt [] ex
--   Just 4
--
--   >>> valueAt [L,L] ex
--   Just 2
--
--   >>> valueAt [L,R] ex
--   Nothing
--
--   >>> valueAt [R,L,R] ex
--   Just 6
--
--   >>> valueAt [L,L,L] ex
--   Nothing
--
valueAt :: Path -> Tree a -> Maybe a
valueAt [] (Node n l r)=Just n
valueAt (x:xs) (Node n l r)=case x of
							 L  ->valueAt xs l
							 R  ->valueAt xs r
valueAt _ End=Nothing






-- | Find a path to a node that contains the given value.
--
--   >>> pathTo 3 (leaf 5)
--   Nothing
--
--   >>> pathTo 5 ex
--   Just [R,L]
--
--   >>> pathTo 6 ex
--   Just [R,L,R]
--
--   >>> pathTo 4 ex
--   Just []
--
--   >>> pathTo 10 ex
--   Nothing
--
--Giving (Eq a) is constraining the set of types that can be used with to the palindrome function. The
--elements of the type a can be tested for equality.
-- when see Eq a=>a->Tree a, is this means have a helper funcation and the result of that help funcation should be Bool?
--sum :: Num a => t a -> a  is" t a "means'[]'?


--this helper funcation can find the node is exist or not.
helper::Eq a=> a->Tree a->Bool
helper x End=False
helper x (Node n l r)= x == n || helper x l || helper x r
-- |x==n = True
-- |otherwise= helper x l || helper x r

-- a is the number that we want to compare with the node number.
--the last path should be the whole path of the node number that we want to find.
stepPath ::Eq a=> a->Path->Tree a->Maybe Path

--The question is :pathTo 5 ex I got:Just [L,R], not Just [R,L]. other cases are work fine.

stepPath x p (Node n l r)= if x==n then Just p
                           else case (helper x l, helper x r) of
                                (False,False)-> Nothing
                                (False,True) -> stepPath x ([R]++p) r
                                (True,False) -> stepPath x ([L]++p) l

--when we get the command, then pass it to get the Path.
pathTo :: Eq a => a -> Tree a -> Maybe Path
pathTo x (Node n l r)
 |x==n =Just []
 |otherwise = (stepPath x [] (Node n l r))


-- | Apply a function to the value at every node in the tree.
--   
--   >>> mapTree odd End
--   End
--
--   >>> mapTree even (Node 5 (leaf 2) (leaf 3))
--   Node False (Node True End End) (Node False End End)
--
--   >>> (mapTree not . mapTree even) (Node 5 (leaf 2) (leaf 3))
--   Node True (Node False End End) (Node True End End)
--
--   >>> mapTree (+10) ex
--   Node 14 (Node 13 (Node 12 End End) End) (Node 17 (Node 15 End (Node 16 End End)) (Node 18 End End))
--
--   >>> ex == (mapTree (subtract 27) . mapTree (+27)) ex
--   True
--
--analyze mapTree (+10)ex, then found the first step should be a funcation works on the number n of Node
mapTree :: (a->b)-> Tree a->Tree b
mapTree f x=case x of
 Node n l r->Node (f n)(mapTree f l)(mapTree f r)
 _ ->End


