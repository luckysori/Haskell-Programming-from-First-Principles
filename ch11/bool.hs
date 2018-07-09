-- pg 629

import Data.Int

data BigSmall =
    Big Bool
  | Small Bool
  deriving (Eq, Show)

-- 1. Cardinality of BigSmall is 4

data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)

-- 2. Cardinality of NumberOrBool is 258. When trying to create a Numba with a numeric literal larger than 127, an out of range warning is given. The same thing happens when a numeric literal smaller than -128 is used. The actual value assigned corresponds to a number within the range taking into account that it wraps around, so that 127 and -128 are consecutive numbers.
