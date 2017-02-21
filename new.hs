doubleMe x = x+x
doubletwo x y = doubleMe x + doubleMe y + 9
doubleBig x = if x > 100 then x*2 else x
doubleBig' x = (if x > 100 then x*2 else x) + 2


b = take 10 [5,8..305]

evens :: [Integer] -> [[Char]] 
evens s = [if x < 10 then "Small even" else "Big even" | x <- s,  not (odd x)]
--folds by recursion
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f acc [] = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)

--take and frop by recursion
take' :: Integer -> [b] -> [b]
take' n [] = []
take' n (x:xs)
    | n <= 0 = []
    | n >= 0 = (x:(take' (n-1) xs))

drop' :: Integer -> [b] -> [b]
drop' n [] = []
drop' n (x:xs)
    | n <= 0 = xs
    | n >= 0 = (drop' (n-1) xs)    

--sum of all numbers less than 1000 which are divisible by 3 or 5
sumsp =  sum(takeWhile(<1000)[x|x<-[1..],x `mod` 3 == 0 || x `mod` 5 ==0])

sumsp' = sum(takeWhile(<1000)(filter p [1..]))  -- using filter
  where p x = (x `mod` 3 == 0 || x `mod` 5 == 0)


onlyUpper :: [Char] -> [Char]
onlyUpper st = [x|x <- st,x `elem` ['A'..'Z']]

doYouGAF :: [[Char]] -> [[Char]] 
doYouGAF st = [if (x == "No" || x== "no") then "No Fs given" else "But why" | x <-  st, x == head st]

 
hoursYouReadBooks :: (Integral a) => a -> a -> String
hoursYouReadBooks hours books
  | ratio <= 1 = "C'mmon give those books some time"
  | ratio <= 2 = "That's quite good!"
  | ratio <= 3 = "Oh you are a devotee"
  | otherwise = "Get OUT! Like Seriously!"
  where ratio = (fromIntegral (hours) / fromIntegral(books))




