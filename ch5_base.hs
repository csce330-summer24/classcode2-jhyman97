pyths :: Int -> [(Int,Int,Int)]
pyths n = [ (x,y,z) | x <-[1..n], y<-[1..n], z<-[1..n], x^2 +y^2 == z^2] 

pyths' n = [ (x,y,z) | x <-[1..n], y<-[1..n], z<-[(max x y) +1..n], x^2 +y^2 == z^2]

pyths'' n = [ (x,y,z) | x <-[1..n], y<-[x..n], z<-[y+1..n], x^2 +y^2 == z^2]

pyths''' n = concat [ if x==y then [(x,y,z)] else [(x,y,z),(y,x,z)] | (x,y,z)<-ps]
    where
        ps = pyths'' n


pyths'''' n = concat [ (\(x,y,z)->[(x,y,z),(y,x,z)]) t  | t<-ps]
    where
        ps = pyths'' n

factors :: Int -> [Int]
factors n = [ x | x<-[1..n], n `mod` x == 0 ]

prime :: Int -> Bool
prime n = factors n == [1,n]


perfect :: Int -> Bool
perfect n = n == sum (init (factors n))

perfects :: Int -> [Int]
perfects n = [ x | x<-[1..n], perfect x]

sp :: Num a => [a] -> [a] -> a
sp xs ys = sum [ xs !! i * ys !! i | i<-[0..length xs - 1] ]

sp' xs ys = sum [ fst xy * snd xy | xy<- zip xs ys ]

sp'' xs ys = sum [ x*y | (x,y)<-zip xs ys]

sp''' [] [] = 0
sp''' (x:xs) (y:ys) = x*y + sp''' xs ys

sp'''' xs ys = foldr (\(x,y) rest-> x*y rest) 0 (zip xs ys)