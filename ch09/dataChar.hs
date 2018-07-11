-- pg 517

import Data.Char

filterUpper = filter isUpper

firstUpper :: String -> Maybe String
firstUpper [] = Nothing
firstUpper (x:xs) = Just $ toUpper x : xs

allUpper :: String -> String
allUpper [] = []
allUpper (x:xs) = toUpper x : allUpper xs

upperHead :: String -> Maybe Char
upperHead [] = Nothing
upperHead (x:_) = Just (toUpper x)

upperHead' xs = toUpper (head xs)

upperHeadPF = toUpper . head
