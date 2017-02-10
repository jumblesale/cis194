-- http://apfelmus.nfshost.com/articles/monoid-fingertree.html

import Prelude hiding (head, (!!))

data Tree v a = Leaf   v a
              | Branch v (Tree v a) (Tree v a)
    deriving (Show)

--      v
--    /   \
--   v     v
--  / \   / \
-- v  v  v   v
-- a  a  a  / \
--         v   v
--         a   a

-- The leaves store the elements of our list from left to right.
toList :: Tree v a -> [a]
toList (Leaf v a) = [a]
toList (Branch _ l r) = toList l ++ toList r

tag :: Tree v a -> v
tag (Leaf v _) = v
tag (Branch v _ _) = v

-- We can also implement the head operation which retrieves the leftmost element
head :: Tree v a -> a
head (Leaf _ a) = a
head (Branch _ l _) = head l

-- annotate each subtree with its size
type Size = Int

--      5
--    /   \
--   2     3
--  / \   / \
-- 1  1  1   2
-- a  a  a  / \
--         1   1
--         a   a

leaf :: a -> Tree Size a
leaf x = Leaf 1 x

branch :: Tree Size a -> Tree Size a -> Tree Size a
branch x y = Branch (tag x + tag y) x y

-- Given size annotations, we can now find the n-th leaf:
(!!) :: Tree Size a -> Int -> a
(!!) (Leaf _ a) 0 = a
(!!) (Branch _ l r) n
    | n < tag l = (!!) l n
    | otherwise = (!!) r (n - tag l)


-- where smaller values are MORE important
--      2
--    /   \
--   4     2
--  / \   / \
-- 16  4  2  8
-- a   a  a / \
--         32  8
--         a   a

type Priority = Int

-- if the priority of the child matches the initial tag, take that path
winner :: Tree Priority a -> a
winner b@(Branch v l r) = winner' v b
    where winner' p (Leaf _ x) = x
          winner' p (Branch v l r)
            | p == tag l = winner' p l
            | otherwise  = winner' p r



