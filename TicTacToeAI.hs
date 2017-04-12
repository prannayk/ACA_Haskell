buildRow :: Char -> Int -> String
buildRow _ 0 = []
buildRow x n = buildRow x (n-1) ++ [x]

buildMatrix :: Int -> Int -> [String]
buildMatrix 0 _ = []
buildMatrix m n = buildMatrix (m-1) n ++ [buildRow '.' n]

start n = buildMatrix n n



showRow :: String -> String
showRow [] = []
showRow (x:xs) = " " ++ [x] ++ showRow xs

showMatrix :: [String] -> String
showMatrix [] = []
showMatrix (xs:xss) = showRow xs ++ "\n" ++ showMatrix xss

d xss = putStr (showMatrix xss)



replace :: [String] -> Int -> Int -> Char -> [String]
replace xss i j x = let (ass, bs:bss) = splitAt (i-1) xss
                        (cs, _:ds) = splitAt (j-1) bs
                    in ass ++ [cs ++ [x] ++ ds] ++ bss

x :: Int -> Int -> [String] -> [String]
x i j xss = replace xss i j 'x'

o :: Int -> Int -> [String] -> [String]
o 0 0 xss = xss
o i j xss = replace xss i j 'o'

dot :: Int -> Int -> [String] -> [String]
dot i j xss = replace xss i j '.'



kthCol :: Int -> [String] -> Int -> String
kthCol k xss m = [xss!!y!!(k-1) | y <- [0..(m-1)]]

diag :: [String] -> Int -> String
diag xss n = [xss!!y!!y | y <- [0..(n-1)]]

revDiag :: [String] -> Int -> String
revDiag xss n = [xss!!y!!(n-y-1) | y <- [0..(n-1)]]

win' :: Int -> [String] -> Int -> Int
win' 0 xss n
      | diag xss n == take n (repeat 'x') = -1
      | diag xss n == take n (repeat 'o') = 1
      | revDiag xss n == take n (repeat 'x') = -1
      | revDiag xss n == take n (repeat 'o') = 1
      | otherwise = 0
win' k xss n
      | kthCol k xss n == take n (repeat 'x') = -1
      | kthCol k xss n == take n (repeat 'o') = 1
      | xss!!(k-1) == take n (repeat 'x') = -1
      | xss!!(k-1) == take n (repeat 'o') = 1
      | otherwise = win' (k-1) xss n
win :: [String] -> Int
win xss = win' (length xss) xss (length xss)



movesOver :: [String] -> Int -> Bool
movesOver xss n = if ([xss!!(i-1)!!(j-1) /= '.' | i <- [1..n], j <- [1..n]] == take (n*n) (repeat True)) then True else False

minimax :: [String] -> Int -> Int -> Bool -> Int
minimax xss n depth tf
      | depth > 4 = 0
      | win xss == 1 = 100-depth
      | win xss == -1 = depth-100
      | movesOver xss n = 0
      | tf == True = maximum [minimax (o i j xss) n (depth+1) (not tf) | i <- [1..n], j <- [1..n], xss!!(i-1)!!(j-1) == '.']
      | otherwise = minimum [minimax (x i j xss) n (depth+1) (not tf) | i <- [1..n], j <- [1..n], xss!!(i-1)!!(j-1) == '.']

whosMax :: [((Int, Int), Int)] -> ((Int, Int), Int)
whosMax [] = ((0,0),0)
whosMax [(pair, x)] = (pair, x)
whosMax ((pair, x):rest)
      | x > (snd restMax) = (pair, x)
      | otherwise = restMax
      where restMax = whosMax rest
      
bestMove' :: [String] -> Int -> (Int, Int)
bestMove' xss n = fst (whosMax [((i, j), minimax (o i j xss) n 0 False) | i <- [1..n], j <- [1..n], xss!!(i-1)!!(j-1) == '.'])
bestMove :: [String] -> (Int, Int)
bestMove xss = bestMove' xss (length xss)

comp :: [String] -> [String]
comp xss = o (fst (bestMove xss)) (snd (bestMove xss)) xss
