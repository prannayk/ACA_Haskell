sm a x y c=do
    let check1 =0
    let check2 =0
    let check3 =0
    let check4 =0
    let check5 =0
    let check6 =0
    let check7 =0
    let check8 =0
    if (a!!x!!y/=2)
    	then do
    		 print "INVALID COORDINATES!! enter unfilled coordinates"
    		 y<-getLine
    		 x<-getLine
    		 sm a (read x::Int) (read y::Int) c
    else return()

    let row=a!!y
    let p1=take x row
    let p2= drop (x+1) row
    let nrow=p1++[c `mod` 2]++p2
    let py1=take y a
    let py2=drop (y+1) a
    let f=py1++[nrow]++py2
    print (f!!0)
    print (f!!1)
    print (f!!2)
    let b = c+1
    check1 <- if (f!!0!!0==f!!0!!1)&&(f!!0!!1==f!!0!!2)&&(f!!0!!2/=2) then return (1) else return (0)
    check2 <- if (f!!1!!0==f!!1!!1)&&(f!!1!!1==f!!1!!2)&&(f!!1!!2/=2) then return (1) else return (0)
    check3 <- if (f!!2!!0==f!!2!!1)&&(f!!2!!1==f!!2!!2)&&(f!!2!!2/=2) then return (1) else return (0)
    check4 <- if (f!!0!!0==f!!1!!0)&&(f!!2!!0==f!!1!!0)&&(f!!1!!0/=2) then return (1) else return (0)
    check5 <- if (f!!0!!1==f!!1!!1)&&(f!!1!!1==f!!2!!1)&&(f!!2!!1/=2) then return (1) else return (0)
    check6 <- if (f!!0!!2==f!!1!!2)&&(f!!2!!2==f!!1!!2)&&(f!!1!!2/=2) then return (1) else return (0)
    check7 <- if (f!!0!!0==f!!1!!1)&&(f!!1!!1==f!!2!!2)&&(f!!2!!2/=2) then return (1) else return (0)
    check8 <- if (f!!0!!2==f!!1!!1)&&(f!!1!!1==f!!2!!0)&&(f!!2!!0/=2) then return (1) else return (0)
    let check = check1+check2+check3+check4+check5+check6+check7+check8
    if (check/=0)
    	then do
             print "Game Over"
             print (c `mod` 2) 
             print "Wins"
    else return()
    print "Please enter x and y coordinates for next turn "
    y<-getLine
    x<-getLine
    if (check==0) then sm f (read x::Int) (read y::Int) b else print ("GAME OVER ")
    
main=do
    let f=[[2,2,2],[2,2,2],[2,2,2]]
    print (f!!0)
    print (f!!1)
    print (f!!2)
    print "Please enter x and y coordinates for first turn "
    x<-getLine
    y<-getLine
    sm f (read x::Int) (read y::Int) 0
