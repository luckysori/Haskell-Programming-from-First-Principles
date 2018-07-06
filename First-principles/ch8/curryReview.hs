-- pg 450

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty = cattyConny "woops"
frappe = flippy "haha"

{-
  1. "woops mrow woohoo!"
  2. "1 mrow haha"
  3. "woops mrow 2 mrow haha"
  4. "woops mrow blue mrow haha"
  5. "pink mrow haha mrow green mrow woops mrow blue"
  6. "are mrow Pugs mrow awesome"
-}
