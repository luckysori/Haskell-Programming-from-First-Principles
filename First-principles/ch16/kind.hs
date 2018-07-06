-- pg 970

{-

1. In a -> a, the kind of a is *
2. In a -> b a -> T (b a), the kind of b is * -> * and the kind of T is * -> *
3. In c a b -> c b a, the kind of c is * -> * -> *

-}

-- pg 994

{-

(.) :: (b -> c) -> (a -> b) -> a -> c

fmap :: Functor f => (m -> n) -> f m -> f n
fmap :: Functor g => (x -> y) -> g x -> g y

(fmap . fmap) ::
(Functor f, Functor g) => ((g x -> g y) -> (f (g x) -> f (g y))) -- already applied
                       -> ((x -> y) -> (g x -> g y))             -- already applied
                       -> (x -> y) -> f (g x) -> f (g y) -- actual type

-}
