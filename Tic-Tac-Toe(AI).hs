import Data.List
printgame state i j n =do
    if(i<n&&j<n-1)then do
        if(state!!i!!j==8) then putStr("__ ")
            else do
                if(state!!i!!j==3000) then putStr(" X ")
                    else putStr(" 0 ")
        printgame state i (j+1) n
        else do
            if(j==n-1&&i<n)then do
                if(state!!i!!j==8) then putStr("__ ")
                    else do
                        if(state!!i!!j==3000) then putStr(" X ")
                            else putStr(" 0 ")
                putStrLn("")
                putStrLn("")
                printgame state (i+1) 0 n
                else
                return ()   

diag1list state 0 n =[state!!0!!0]
diag1list state i n = [state!!i!!i] ++ diag1list state (i-1) n
diag2list state 0 n = [state!!0!!(n-1)]
diag2list state i n = [state !!i!!(n-1-i)] ++ diag2list state (i-1) n






max2 :: Int -> Int -> IO Int
max2 a b = do
	if (a>b)then return a
		else return b

minimax :: [[Int]] -> Int -> Int -> Int -> Int -> IO Int
minimax state i n bestscore besti= do -- moves are list of all valid moves
        movesi <- getmovei state n 0 0 []
        movesj <- getmovej state n 0 0 []
        let num = length movesj
        if (i<num) then do
                let clone1 = change state (movesi!!i) (movesj!!i) 3000
                score1 <- minplay clone1 0 n  (100000) (0) 0-- do a proper call 
                let clone2 = change state (movesi!!i) (movesj!!i) 3001
                score2 <- maxplay clone2 0 n  (-100000) (0) 0-- do a proper call 
                score <- max2 score1 score2
                if(score>bestscore) then do
                    minimax state (i+1) n score i
                    else minimax state (i+1) n bestscore besti
            else do  
                return besti


minplay ::[[Int]] -> Int -> Int -> Int ->Int -> Int -> IO Int
minplay state i n bestscore besti depth= do -- moves are list of all valid moves
        movesi <- getmovei state n 0 0 []
        movesj <- getmovej state n 0 0 []
        let num = length movesj
        if(i==num || depth >8) then do
            res <- evaluate state n
            return res
            else do
                if (i<num) then do
                    let clone = change state (movesi!!i) (movesj!!i) (3001)
                    score <- maxplay clone 0 n (-100000) (0) (1+depth) -- do a proper call 
                    if(score<bestscore) then do
                        minplay state (i+1) n score besti depth
                        else minplay state  (i+1) n bestscore i depth
                else return bestscore


maxplay ::[[Int]] -> Int -> Int -> Int  ->Int -> Int -> IO Int
maxplay state i n bestscore besti depth= do -- moves are list of all valid moves
        movesi <- getmovei state n 0 0 []
        movesj <- getmovej state n 0 0 []
        let num = length movesj
        if(i==num || depth > 8) then do
            res <- evaluate state n
            return res
            else do
                if (i<num) then do
                    let clone = change state (movesi!!i) (movesj!!i) (3000)
                    score <- minplay clone 0 n 100000 (0) (1+depth)-- do a proper call 
                    if(score>bestscore) then do
                        maxplay state (i+1) n score besti depth
                        else maxplay state (i+1) n bestscore i depth
                else return bestscore
change::[[Int]]->Int->Int->Int->[[Int]]
change state i j val=(take i state)++[(take j (state!!i))++[val]++(drop (j+1) (state!!i))]++(drop (i+1) state)

getmovei :: [[Int]] -> Int -> Int -> Int -> [Int] -> IO [Int]
getmovei state n i j list=do
    if(i==n)then return list
        else do
            if(j==n-1) then do
                if(state!!i!!j==8) then getmovei state n (i+1) 0 (list ++ [i])
                    else getmovei state n (i+1) 0 (list)
                else do
                    if(state!!i!!j==8) then getmovei state n (i) (j+1) (list ++ [i])
                        else getmovei state n (i) (j+1) (list)

getmovej :: [[Int]] -> Int -> Int -> Int -> [Int] -> IO [Int]
getmovej state n i j list=do
    if(i==n)then return list
        else do
            if(j==n-1) then do
                if(state!!i!!j==8) then getmovej state n (i+1) 0 (list ++ [j])
                    else getmovej state n (i+1) 0 (list)
                else do
                    if(state!!i!!j==8) then getmovej state n (i) (j+1) (list ++ [j])
                        else getmovej state n (i) (j+1) (list)


            


dlist1 board 0 n=[board!!0!!0]
dlist1 board i n=[board!!i!!i]++dlist1 board (i-1) n
dlist2 board 0 n=[board!!0!!(n-1)]
dlist2 board i n=[board!!i!!(n-i-1)]++dlist2 board (i-1) n
-------------------------------------------------------------------------------
ones row=length (filter (==3000) row)
negs row=length (filter (==(3001)) row)
scr o n=(20^o)-(20^n)

scor::[[Int]]->Int->Int->Int->IO Int
scor a i s dim=do
    if(i<dim) then do
        let b=s+scr (ones (a!!i)) (negs (a!!i))
        scor a (i+1) b dim
            else return s
evaluate::[[Int]]->Int->IO Int
evaluate a dim= do
    t1<-scor a 0 0 dim
    t2<-scor (transpose a) 0 0 dim
    let o=ones (dlist1 a (dim-1) (dim))
    let n=negs (dlist1 a (dim-1) (dim))
    let t3=scr o n
    let o=ones (dlist2 a (dim-1) (dim))
    let n=negs (dlist2 a (dim-1) (dim))
    let t4=scr o n
    let m=(t1+t2+t3+t4)
    return m

input state n= do
    str2 <- getLine
    let i=read str2::Int
    str3 <- getLine
    let j=read str3::Int
    let clone = change state i j 3000
    b <- minimax clone 0 n 0 0
    movesi <- getmovei clone n 0 0 []
    movesj <- getmovej clone n 0 0 []
    let num = length movesj
    if(num==0) then do
    	printgame clone 0 0 n
    	print "GAME"
    	else do
    		print "Com 's turn"
    		let clone2 = change clone (movesi!!b) (movesj!!b) 3001
    		printgame clone2 0 0 n
    		input clone2 n


main = do
    print "please enter dimension of the Grid"
    str<-getLine
    let n = read str::Int
    let state = (replicate n) ((replicate n) 8)
    printgame state 0 0 n
    input state n 
    print "OVER"
