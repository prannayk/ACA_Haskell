import Data.Char
import Data.List
import System.IO
import Data.Int

-- Reading a number

getNum :: String -> IO Int
getNum prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                      return (read xs)
                   else
                      do putStrLn "NOT A VALID NUMBER"
                         getNum prompt


size :: Int
size = 3

type Playgrid = [[Player]]

data Player = O | B | X -- X and O, B is for Blank 
              deriving (Eq, Ord, Show)

changeturn :: Player -> Player
changeturn O = X
changeturn B = B
changeturn X = O


blank :: Playgrid --creates blank Playgrid
blank = replicate size (replicate size B)

full :: Playgrid -> Bool--will check for full Playgrid
full = all (/= B) . concat

wins :: Player -> Playgrid -> Bool--checks for win condition
wins p g = any checkr (rows ++ cols ++ dias)--metrue if any return true
           where
              checkr = all (== p)
              rows = g
              cols = transpose g
              dias = [findDiag g, findDiag (map reverse g)]

findDiag :: Playgrid -> [Player]
findDiag g = [g !! n !! n | n <- [0..size-1]]

-- !!Plan to add gridlines to the Playgrid output, thus two seperate row and player str of strings!! 

putPlaygrid :: Playgrid -> IO ()
putPlaygrid =
   putStrLn . unlines . concat .map rowStr--unline inserts new line character

rowStr :: [Player] -> [String]
rowStr = beside . map playerStr
          where
             beside = foldr1 (zipWith (++)) -- joins the strings from last two elements 

playerStr :: Player -> [String]
playerStr O = ["   ", " O ", "   "]
playerStr B = ["   ", "   ", "   "]
playerStr X = ["   ", " X ", "   "]

-- For moves 

isValid :: Playgrid -> Int -> Bool
isValid g i = (i >= 0) && (i < (size^2)) && (concat g !! i == B)

move:: Playgrid -> Int -> Player -> [Playgrid]
move g i p =
   if isValid g i then [cutt size (xs ++ [p] ++ ys)] else []
   where (xs,B:ys) = splitAt i (concat g)--splitAt splits list at i

cutt :: Int -> [a] -> [[a]]--creates sublists of size n from original lists
cutt n [] = []
cutt n xs = take n xs : cutt n (drop n xs)



tictactoe :: IO ()
tictactoe = do gameloop blank O

gameloop :: Playgrid -> Player -> IO ()
gameloop g p = do putPlaygrid g
                  gameloop' g p

gameloop' :: Playgrid -> Player -> IO ()
gameloop' g p 
         | wins O g  = putStrLn "Player O is winner!\n"
         | wins X g  = putStrLn "Player X is winner!\n"
         | full g    = putStrLn "GAME DRAW!\n"
         | otherwise =
              do num <- getNum (prompts' p)
                 case move g num p of
                    []   -> do putStrLn "Invalid Move"
                               gameloop' g p
                    [g'] -> gameloop g' (changeturn p)

prompts' :: Player -> String
prompts' p = "Player " ++ show p ++ ", enter your move: "

