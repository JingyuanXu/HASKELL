--Jingyuan Xu   id: 932428597
module HW1 where

--
-- * Part 1: Binary trees
--

-- | Integer-labeled binary trees.
data Tree = Node Int Tree Tree   -- ^ Internal nodes
          | Leaf Int             -- ^ Leaf nodes
  deriving (Eq,Show)


-- | An example binary tree, which will be used in tests.
t1 :: Tree
t1 = Node 1 (Node 2 (Node 3 (Leaf 4) (Leaf 5))
                    (Leaf 6))
            (Node 7 (Leaf 8) (Leaf 9))

-- | Another example binary tree, used in tests.
t2 :: Tree
t2 = Node 6 (Node 2 (Leaf 1) (Node 4 (Leaf 3) (Leaf 5)))
            (Node 8 (Leaf 7) (Leaf 9))


-- | The integer at the left-most node of a binary tree.
--
--   >>> leftmost (Leaf 3)
--   3
--
--   >>> leftmost (Node 5 (Leaf 6) (Leaf 7))
--   6
--   
--   >>> leftmost t1
--   4
--
--   >>> leftmost t2
--   1
--
leftmost :: Tree -> Int
leftmost (Leaf i)     = i
leftmost (Node _ l _) = leftmost l


-- | The integer at the right-most node of a binary tree.
--
--   >>> rightmost (Leaf 3)
--   3
--
--   >>> rightmost (Node 5 (Leaf 6) (Leaf 7))
--   7
--   
--   >>> rightmost t1
--   9
--
--   >>> rightmost t2
--   9
--
rightmost :: Tree->Int
rightmost (Leaf i)=i
rightmost (Node _ _ r)=rightmost r


-- | Get the maximum integer from a binary tree.
--
--   >>> maxInt (Leaf 3)
--   3
--
--   >>> maxInt (Node 5 (Leaf 4) (Leaf 2))
--   5
--
--   >>> maxInt (Node 5 (Leaf 7) (Leaf 2))
--   7
--
--   >>> maxInt t1
--   9
--
--   >>> maxInt t2
--   9
--
maxInt :: Tree->Int
maxInt (Leaf i)=i
maxInt (Node x l r)= max x (max (maxInt l) (maxInt r))


-- | Get the minimum integer from a binary tree.
--
--   >>> minInt (Leaf 3)
--   3
--
--   >>> minInt (Node 2 (Leaf 5) (Leaf 4))
--   2
--
--   >>> minInt (Node 5 (Leaf 4) (Leaf 7))
--   4
--
--   >>> minInt t1
--   1
--
--   >>> minInt t2
--   1
--
minInt :: Tree->Int
minInt (Leaf i)=i
minInt (Node x l r)= min x (min (minInt l) (minInt r))





-- | Get the sum of the integers in a binary tree.
--
--   >>> sumInts (Leaf 3)
--   3
--
--   >>> sumInts (Node 2 (Leaf 5) (Leaf 4))
--   11
--
--   >>> sumInts t1
--   45
--
--   >>> sumInts (Node 10 t1 t2)
--   100
--
sumInts ::Tree->Int
sumInts (Leaf i)=i
sumInts (Node x l r)= add x (add (sumInts l) (sumInts r))


add                     :: Int -> Int -> Int
add x y                 =  x + y



-- | The list of integers encountered by a pre-order traversal of the tree.
--
--   >>> preorder (Leaf 3)
--   [3]
--
--   >>> preorder (Node 5 (Leaf 6) (Leaf 7))
--   [5,6,7]
--
--   >>> preorder t1
--   [1,2,3,4,5,6,7,8,9]
--
--   >>> preorder t2
--   [6,2,1,4,3,5,8,7,9]
--

preorder  :: Tree -> [Int]
preorder   (Leaf i) = [i]
preorder  (Node x l r) = [x] ++ preorder  l  ++ preorder r


-- | The list of integers encountered by an in-order traversal of the tree.
--
--   >>> inorder (Leaf 3)
--   [3]
--
--   >>> inorder (Node 5 (Leaf 6) (Leaf 7))
--   [6,5,7]
--
--   >>> inorder t1
--   [4,3,5,2,6,1,8,7,9]
--
--   >>> inorder t2
--   [1,2,3,4,5,6,7,8,9]
--   

inorder  :: Tree -> [Int]
inorder   (Leaf i) = [i]
inorder  (Node x l r) =  inorder  l++ [x]  ++ inorder r

-- | Check whether a binary tree is a binary search tree.
--
--   >>> isBST (Leaf 3)
--   True
--
--   >>> isBST (Node 5 (Leaf 6) (Leaf 7))
--   False
--   
--   >>> isBST t1
--   False
--
--   >>> isBST t2
--   True
--   
isBST :: Tree->Bool
--isBST ( x: l :r)
--	|(compare l x) == LT==True
--	|(compare r x) == GT==True
--	|otherwise= False
isBST (Leaf i)=True
isBST (Node x l r)=case (isBST l, isBST r) of
                 (True, True)-> (compare (minInt l) x) == LT && (compare (maxInt r) x) == GT
                 _	         -> False

-- | Check whether a number is contained in a binary search tree.
--   (You may assume that the given tree is a binary search tree.)
--
--   >>> inBST 2 (Node 5 (Leaf 2) (Leaf 7))
--   True
--
--   >>> inBST 3 (Node 5 (Leaf 2) (Leaf 7))
--   False
--
--   >>> inBST 4 t2
--   True
--
--   >>> inBST 10 t2
--   False
--   
inBST :: Int-> Tree->Bool
inBST n (Leaf i)
	  |n==i = True
      |otherwise = False
inBST n (Node x l r)
      |x>n =inBST n l
      |x<n =inBST n r
      |otherwise =True

--inBST n (Node x l r)=case (inBST n l, inBST n r) of
--					(True, True)-> (compare (minInt l) n)==EQ ||(compare (maxInt r) n)==EQ
--					_          -> True

--
-- * Part 2: Run-length lists
--


-- | Convert a regular list into a run-length list.
--
--   >>> compress [1,1,1,2,3,3,3,1,2,2,2,2]
--   [(3,1),(1,2),(3,3),(1,1),(4,2)]
--
--   >>> compress "Mississippi"
--   [(1,'M'),(1,'i'),(2,'s'),(1,'i'),(2,'s'),(1,'i'),(2,'p'),(1,'i')]
--
compress :: Eq a => [a] -> [(Int,a)]
compress [] = []
compress [x] = [(1,x)]
compress (x:y:xs)
 | x == y = succHead (compress (y:xs))
 | otherwise = (1,x) : compress (y:xs)

succHead :: [(Int,a)] -> [(Int,a)]
succHead [] = []
succHead ((n,x):ps) = (n+1,x):ps


-- | Convert a run-length list back into a regular list.
--
--   >>> decompress [(5,'a'),(3,'b'),(4,'c'),(1,'a'),(2,'b')]
--   "aaaaabbbccccabb"
--
decompress :: [(Int,a)] -> [a]
decompress [] = []
decompress ((n,x):ps) = (repetition n x) ++ decompress ps

repetition :: Int -> a -> [a]
repetition 0 _ = []
repetition n x = x:(repetition (n-1) x)

