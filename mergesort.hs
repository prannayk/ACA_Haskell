fsthalf :: [a] -> [a]
fsthalf xs = take (length xs `div` 2) xs

sndhalf :: [b] -> [b]
sndhalf xs = drop (length xs `div` 2) xs

merge :: Ord c => [c] -> [c] -> [c]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x <= y
                      then x:(merge xs (y:ys))
                      else y:(merge (x:xs) ys)

mergesort :: Ord d => [d] -> [d]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort (fsthalf xs)) (mergesort (sndhalf xs))

