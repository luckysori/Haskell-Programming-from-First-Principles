-- pg 579

stops = "pbtdkg"
vowels = "aeiou"

stopVowelStop = [ (x, y, z) | x <- stops, y <- vowels, z <- stops]
pVowelStop = [ ('p', y, z) | y <- vowels, z <- stops]

nouns = ["rocks", "rabbits", "fools", "pots", "ants", "wizards"]
verbs = ["eat", "steal", "have", "make", "obey"]
phrases = [ (x, y, z) | x <- nouns, y <- verbs, z <- nouns]

seekritFunc x =
  div (sum (map length (words x))) (length (words x))

avgWordLength x =
  fromIntegral (sum (map length (words x))) / (fromIntegral (length (words x)))
