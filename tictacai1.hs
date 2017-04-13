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
findDiag nums g = [g !! n !! n | n <- [0..nums-1]]--puts diagonal elements into a list

-- !!Plan to add gridlines to the Playgrid output, thus two seperate row and player str of strings!! 
--functions to print the grid 
putPlaygrid :: Playgrid -> IO ()
putPlaygrid = mapM_ (putStrLn . intercalate " " . map show) 
-- For moves 

isValid :: Int -> Playgrid -> Int -> Bool
isValid nums g i = (i >= 0) && (i < (nums^2)) && (concat g !! i == B)

move:: Int -> Playgrid -> Int -> Player -> [Playgrid]--changes the ith element of the playgrid (concatonated).
--Splits concatoated playgrid into parts before and after the ith element then merge those parts adding the player letter in between.
move nums g i p =  -- nums is grid size, g is playgrid, i is move number entered and p is player (X or O)
   if isValid nums g i then [cutt nums (xs ++ [p] ++ ys)] else []--cutt nums restores the concatonated list into lists of rows
   where (xs,B:ys) = splitAt i (concat g)--splitAt splits list at i and makes a list containing those two split parts

cutt :: Int -> [a] -> [[a]]--creates sublists of size n from original lists
cutt n [] = []
cutt n xs = take n xs : cutt n (drop n xs)--takes n makes a list then takes n from the remainder and puts all those lists into a list

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
         | (p == X) = gameloop sizes (automoveX g sizes 10) O
         | otherwise =
              do num <- getNum (prompts' p)
                 case move sizes g num p of--generates the new grid after num entered by player p
                    []   -> do putStrLn "Invalid Move"
                               gameloop' sizes g p
                    [g'] -> gameloop sizes g' (changeturn p)
--prompts user to enter move
prompts' :: Player -> String
prompts' p = "Player " ++ show p ++ ", enter your move: "

data Tree a = Node a [Tree a]
              deriving Show

possiblemoves :: Int -> Playgrid -> Player -> [Playgrid]
possiblemoves size g p
              | wins size p g = []
              | full g = []
              | otherwise = concat [move size g i p | i <- [0 .. ((size^2)-1)]]
-- move gives a single playgrid inside a list so to take that playgrid out of the list we need to concat 

scorerow :: [Player] -> Int
scorerow row = getscore(foldl (\(score,countx,counto) player -> if (player == X ) then (score + (10^(countx+1)),countx+1,0) else if (player == O) then (score - 10^(counto+1),0,counto+1) else (score,0,0)) (0,0,0) (row))

scoreFGrid :: Int -> Playgrid-> Int
scoreFGrid size g  
                | wins size X g = 100000000000000000
                | wins size O g = -100000000000000000 
                |otherwise = (scorerowWise g) +  (scorerowWise (transpose g)) + (scorerow(findDiag size g)) + (scorerow(findDiag size (map reverse g)))

scorerowWise :: Playgrid -> Int
scorerowWise g = foldl (\acc row -> acc + scorerow row) 0 g

maxlist :: Num a => Ord a => [a] -> a
maxlist p = foldl max 0 p

minlist :: Num a =>  Ord a => [a] -> a
minlist p = foldl min 0 p

getfirst :: (a,b) -> a
getfirst (x,y) = x

getsecond :: (a,b) -> b
getsecond (x,y) = y   

--without using trees
gameMtree :: Int-> Player -> Int-> Playgrid-> (Playgrid,Int)--can be easily edited to add playerturn ton the tuple if required by future functions
gameMtree size p depth game  
               | (not (isover size game)) && (depth > 0) =  if (p == X) then (game, maxlist $ map getsecond $ map (gameMtree size (changeturn p) (depth-1)) [g'| g' <- possiblemoves size game p ])  else (game, minlist $ map getsecond $ map (gameMtree size (changeturn p) (depth-1)) [g'| g' <- possiblemoves size game p ])   --Node (g,score' size p g) [ gameStree size (g',score' size (changeturn p) g') (changeturn p) (depth-1)| g' <- possiblemoves size g p ]
               | (isover size game) || (depth == 0) = (game, scoreFGrid size game) --Node (g,scoreFGrid g) []--[gameStree size (g',scoreFGrid g') (changeturn p) | g' <- possiblemoves size g p ]
 

--move selector
automoveX :: Playgrid -> Int -> Int -> Playgrid
automoveX g size depth  = getfirst $ getmaxpair $ map (gameMtree size O (depth)) [g'| g' <- possiblemoves size g X ]
 
--have to create isover function
--need to check if we can apply getsecond directly over the result of gameMtree i.e. wether Node a [] is same as a.
--I expect it shouldn't be the same, so make a function that given a Tree data type, gets the node of the tree.
--create getfirst and getmaxpair

getmaxpair :: Num b => Ord b=> [(a,b)] -> (a,b)
getmaxpair l = head [(a,b) | (a,b) <- l ] 
                 where b = maxlist(map getsecond l)

isover :: Int -> Playgrid -> Bool
isover size game = (wins size O game) || (wins size X game)
---------------------------------------------------------------------------------------------------------------------
--unused code


gametree :: Int -> Playgrid -> Player -> Tree Playgrid
gametree size g p = Node g [ gametree size g' (changeturn p) | g' <- possiblemoves size g p]           

findrow :: Int->  Int -> Int
findrow size x = x `div` size 

getrow :: Int -> Int -> Playgrid -> [Player]
getrow size x g = g!!(findrow size x)

getscore (x,_,_) = x

findcol :: Int -> Int -> Int
findcol size x = (x) `mod` size

getcol :: Int -> Int -> Playgrid -> [Player]
getcol size x g = (transpose g)!!(findcol size x)



scoreLeaddiag :: Int -> Int -> Playgrid -> Int
scoreLeaddiag size i g = if (i `elem` [0,(size+1)..((size^2) -1)]) then scorerow(findDiag size g) else 0

scoreOtherdiag :: Int -> Int -> Playgrid -> Int
scoreOtherdiag size i g = if (i `elem` [(size-1),(2*(size-1))..(size * (size-1))]) then scorerow(findDiag size (map reverse g)) else 0

scoregrid :: Int -> Int -> Playgrid -> Int
scoregrid i size g = scorerow (getrow size i g) + scorerow (getcol size i g) + scoreLeaddiag size i g + scoreOtherdiag size i g

--scoreTree size (g,score,depth) playerturn limit = Node (g,score,depth) [Tree (g',score',depth-1) where g' = (possiblemoves size g playerturn)]

gameStree :: Int -> (Playgrid,Int) -> Player -> Int-> Tree (Playgrid,Int)
gameStree size (g,score) p depth
               | wins size X (getfirst (g,score)) =  Node (g,scoreFGrid size g) []
               | wins size O (getfirst (g,score)) = Node (g,scoreFGrid size g) []
               | (depth > 0) =  Node (g,score' size p g) [ gameStree size (g',score' size (changeturn p) g') (changeturn p) (depth-1)| g' <- possiblemoves size g p ]
               | (depth == 0) =  Node (g,scoreFGrid size g) []--[gameStree size (g',scoreFGrid size g') (changeturn p) | g' <- possiblemoves size g p ]
                                     where score' size p g = if p==X then maxlist (map (scoreFGrid size) [g'| g' <- possiblemoves size g p ]) else minlist (map (scoreFGrid size) [g'| g' <- possiblemoves size g p])
--or try score tree given a gametree 
--given infor is size,playgrid,playerturn,score at that pt
