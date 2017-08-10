module Hasklet1 where


-- | A generic binary tree with values at internal nodes.
data Tree a = Node a (Tree a) (Tree a)
            | Leaf
  deriving (Eq,Show)

-- Some example trees.
t1, t2, t3 :: Tree Int
t1 = Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf)
t2 = Node 4 t1 (Node 5 Leaf (Node 6 Leaf Leaf))
t3 = Node 7 t1 t2


-- | Sum the integers in a binary tree.
--
--   >>> tsum Leaf
--   0
--
--   >>> tsum t1
--   6
--
--   >>> tsum t2
--   21
--
--   >>> tsum t3
--   34
--
tsum :: Tree Int -> Int
tsum Leaf =0
tsum (Node n l r)=n+ tsum l + tsum r


-- | Check if a given element is contained in a tree.
--
--   >>> contains 5 t1
--   False
--
--   >>> contains 5 t2
--   True
--
--   >>> contains 5 t3
--   True
--
contains :: Eq a => a -> Tree a -> Bool
contains a (Node n l r)= (a==n)||contains a l || contains a r
contains _ Leaf = False


-- | Fold over a binary tree.
tfold :: (a->b->b->b)->b->Tree a ->b
tfold f b (Node n l r) =f n (tfold f b l) (tfold f b r)
tfold f b Leaf = b


-- | Sum the integers in a binary tree, this time using tfold.
--
--   >>> tsum' Leaf
--   0
--
--   >>> tsum' t1
--   6
--
--   >>> tsum' t2
--   21
--
--   >>> tsum' t3
--   34
--   
tsum' :: Tree Int -> Int
tsum' = tfold(\i l r-> i+l+r) 0


-- | Check if a given element is contained in a tree, this time using tfold.
--
--   >>> contains' 5 t1
--   False
--
--   >>> contains' 5 t2
--   True
--
--   >>> contains' 5 t3
--   True
--
contains' :: Eq a => a -> Tree a -> Bool
contains' a= tfold (\n l r -> (a==n)||l|| r)False


-- | Return a nested list where the first list contains the value at the root,
--   the second list contains the values at its children, the third list
--   contains the values at the next level down the tree, and so on.
--
--   >>> levels Leaf
--   []
--   
--   >>> levels t1
--   [[1],[2,3]]
--
--   >>> levels t2
--   [[4],[1,5],[2,3,6]]
--
--   >>> levels t3
--   [[7],[1,4],[2,3,1,5],[2,3,6]]
--   
levels :: Tree a -> [[a]]
levels (Node n l r)= [n]: zipWith (++) (levels l) (levels r)
