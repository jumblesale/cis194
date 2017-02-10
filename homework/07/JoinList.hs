import Data.Monoid
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = (Append (tag x <> tag y) x y)

-- gets the annotation at the root of a JoinList
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

exampleJoinList =  Append (Product 210)
   (Append (Product 30)
     (Single (Product 5) 'y')
     (Append (Product 6)
       (Single (Product 2) 'e')
       (Single (Product 3) 'a')))
   (Single (Product 7) 'h')


getSizeFromTag :: (Sized b, Monoid b) => JoinList b a -> Int
getSizeFromTag = getSize . size . tag

-- indexJ finds the JoinList element at the specified index. If the index
-- is out of bounds, the function returns Nothing.
-- By an index in a JoinList we mean the index in the list that it represents
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty        = Nothing
indexJ _ (Single _ x) = Just x
indexJ i (Append m _ _)   | i < 0                   = Nothing
indexJ i x@(Append m _ _) | i == (getSizeFromTag x) = Nothing -- what
indexJ i (Append m l r)
    | i < getSizeFromTag l = indexJ (i - getSizeFromTag l) l
    | otherwise            = indexJ (getSizeFromTag r - i) r

prop_getZerothElementOfASingle = (Just 'y') == indexJ 0 (Single (Size 3) 'y')
