import Data.Char
import Data.List
import System.IO
import Data.Int
import System.TimeIt

-- Reading a number
getNum :: String -> IO Int
getNum prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                      return (read xs)
                   else
                      do putStrLn "NOT A VALID NUMBER"
                         getNum prompt

--setting up grid and turn changer

type Playgrid = [[Player]]

data Player = O | B | X -- X and O, B is for Blank 
              deriving (Eq, Ord, Show)

changeturn :: Player -> Player
changeturn O = X
changeturn B = B
changeturn X = O


blank :: Int -> Playgrid --creates blank Playgrid
blank num = replicate num (replicate num B)

full :: Playgrid -> Bool--will check for full Playgrid
full = all (/= B) . concat

wins :: Int -> Player -> Playgrid -> Bool--checks for win condition
wins nums p g = any checkr (rows ++ cols ++ dias)--true if any return true
           where
              checkr = all (== p)
              rows = g
              cols = transpose g
              dias = [findDiag nums g, findDiag nums (map reverse g)]

findDiag :: Int -> Playgrid -> [Player]
findDiag nums g = [g !! n !! n | n <- [0..nums-1]]

-- !!Plan to add gridlines to the Playgrid output, thus two seperate row and player str of strings!! 
--functions to print the grid 
putPlaygrid :: Playgrid -> IO ()
putPlaygrid = mapM_ (putStrLn . intercalate " " . map show) 
-- For moves 

isValid :: Int -> Playgrid -> Int -> Bool
isValid nums g i = (i >= 0) && (i < (nums^2)) && (concat g !! i == B)

move:: Int -> Playgrid -> Int -> Player -> [Playgrid]
move nums g i p =
   if isValid nums g i then [cutt nums (xs ++ [p] ++ ys)] else []
   where (xs,B:ys) = splitAt i (concat g)--splitAt splits list at i

cutt :: Int -> [a] -> [[a]]--creates sublists of size n from original lists
cutt n [] = []
cutt n xs = take n xs : cutt n (drop n xs)

--main function to take in grid size and runs gameloop
main :: IO ()
main = do sizes <- getNum "Enter the grid size : "
          timeIt $ gameloop sizes (blank sizes) O
--prints the playGrid and calls gameloop'
gameloop :: Int -> Playgrid -> Player -> IO ()
gameloop sizes g p = do putPlaygrid g
                        gameloop' sizes g p
--checks game terminate conditions and continues game if not true
gameloop' :: Int -> Playgrid -> Player -> IO ()
gameloop' sizes g p 
         | wins sizes O g  = putStrLn "Player O is winner!\n"
         | wins sizes X g  = putStrLn "Player X is winner!\n"
         | full g    = putStrLn "GAME DRAW!\n"
         | otherwise =
              do num <- getNum (prompts' p)
                 case move sizes g num p of
                    []   -> do putStrLn "Invalid Move"
                               gameloop' sizes g p
                    [g'] -> gameloop sizes g' (changeturn p)
--prompts user to enter move
prompts' :: Player -> String
prompts' p = "Player " ++ show p ++ ", enter your move: "

