and' [] = True
and' (b:bs) = b && and' bs

and'' [] = True
and'' (False:_) = False
and'' (True:bs) = and'' bs

and''' bs = foldr (&&) True bs

and'''' :: [Bool] -> Bool
and'''' = foldr (&&) True

concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

replicate' 0 _ = []
replicate' n c = c : replicate' (n-1) c

repeat' c = c : repeat' c

replicate'' n c = take n (repeat' c)

(!!!) :: [a] -> Int -> a
(x:_) !!! 0 = x
(_:xs) !!! n = xs !!! (n-1)

elem' :: Eq a -> a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys)
    | x == y    = True
    | otherwise = elem' x ys


elem'' :: Eq a -> a -> [a] -> Bool
elem'' _ [] = False
elem'' x (y:ys) = if x == y then true else elem'' x ys

merge :: Ord a -> [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x: merge xs (y:ys)
    | otherwise = y: merge (x:xs) ys

msort :: Ord a -> [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
    where
        half = length xs `div` 2
        (left,right) = splitAt half xs