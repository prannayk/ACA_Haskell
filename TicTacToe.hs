row :: Char -> Int -> String
row _ 0 = []
row x n = row x (n-1) ++ [x]

matrix :: Int -> Int -> [String]
matrix 0 _ = []
matrix m n = matrix (m-1) n ++ [row '.' n]

start n = matrix n n



rowdisplay :: String -> String
rowdisplay [] = []
rowdisplay (x:xs) = " " ++ [x] ++ rowdisplay xs

display :: [String] -> String
display [] = []
display (x:xs) = rowdisplay x ++ "\n" ++ display xs

d xs = putStr (display xs)



replace :: [String] -> Int -> Int -> Char -> [String]
replace xs i j x = let (as, b:bs) = splitAt (i-1) xs
                       (cs, _:ds) = splitAt (j-1) b
                   in as ++ [cs ++ [x] ++ ds] ++ bs

x :: Int -> Int -> [String] -> [String]
x i j xs = replace xs i j 'x'

o :: Int -> Int -> [String] -> [String]
o i j xs = replace xs i j 'o'
