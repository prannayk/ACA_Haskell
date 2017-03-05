isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

make :: Int -> String
make 0 = []
make n
   | n < 10 =  make (n-1) ++ "  " ++ show n ++ " "
   | n < 100 = make (n-1) ++ "  " ++ show n
   | otherwise = make (n-1) ++ " " ++ show n

start n = make (n*n)



display :: Int -> String -> String
display _ [] = []
display n xs = (take (4*n) xs) ++ "\n\n" ++ display n (drop (4*n) xs)

d xs = putStr (display (isqrt ((length xs) `div` 4)) xs)



replace :: String -> Int -> [Char] -> String
replace xs n cs = let (as, _:_:_:bs) = splitAt n xs
                  in as ++ cs ++ bs

x :: Int -> String -> String
x n xs = replace xs (4*n-3) " X "

o :: Int -> String -> String
o n xs = replace xs (4*n-3) " O "
