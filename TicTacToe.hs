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
o i j xss = replace xss i j 'o'

dot :: Int -> Int -> [String] -> [String]
dot i j xss = replace xss i j '.'