import           Data.Foldable
import           Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' xs = getSum $ foldMap Sum xs

product' :: (Foldable t, Num a) => t a -> a
product' xs = getProduct $ foldMap Product xs

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x xs = getAny $ foldMap (\y -> Any $ y == x) xs

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' xs
  | null xs = Nothing
  | otherwise = Just $ foldr1 min xs

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' xs
  | null xs = Nothing
  | otherwise = Just $ foldr1 max xs

null' :: (Foldable t) => t a -> Bool
null' = (== 0) . length

length' :: (Foldable t) => t a -> Int
length' xs = getSum $ foldMap (const $ Sum 1) xs

toList' :: (Foldable t) => t a -> [a]
toList' xs = foldr (:) [] xs

fold' :: (Foldable t, Monoid m) => t m -> m
fold' xs = foldr (<>) mempty xs

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f xs = foldr (mappend . f) mempty xs
