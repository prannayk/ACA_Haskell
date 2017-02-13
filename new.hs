doubleMe x = x+x
doubletwo x y = doubleMe x + doubleMe y + 9
doubleBig x = if x > 100 then x*2 else x
doubleBig' x = (if x > 100 then x*2 else x) + 2


b = take 10 [5,8..305]

evens :: [Integer] -> [[Char]] 
evens s = [if x < 10 then "Small even" else "Big even" | x <- s,  not (odd x)]


onlyUpper :: [Char] -> [Char]
onlyUpper st = [x|x <- st,x `elem` ['A'..'Z']]
