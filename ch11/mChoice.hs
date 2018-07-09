-- pg 690

{-

1. Weekday is a) a type with five data constructors
2. The type of the function f is c) f :: Weekday -> String
3. Types defines with the data keyword b) must begin with a capital letter
4. The function g xs = xs !! (length xs - 1) c) delivers the final element of xs

-}

data Weekday =
  Monday | Tuesday | Wednesday | Thursday | Friday deriving Show

f Friday = "Miller Time"
