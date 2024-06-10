filtmap' p f xs = [f x | x <- xs, p x]

filtmap'' p f xs = map f (filter p xs)

filtmap''' p f = (map f).(filter p)

map' f xs = [ f x | x <- xs]

map'' f xs = foldr (\x rest -> f x : rest) [] xs

map''' f xs = foldr (\x rest -> [f x] ++ rest) [] xs

map'''' f xs = foldl (\prev x -> prev ++ [f x]) []

filter' p xs = [ x | x <- xs, p x]

filter'' p xs = foldr (\x rest -> if p x then x:rest else rest) [] xs

max' :: Ord a => [a] -> a
max' (x:xs) = foldr (\y max_rest -> if y>max_rest then y else max_rest) x xs

max'' :: Ord a => [a] -> a
max'' xs = foldr1 (\y max_rest -> if y>max_rest then y else max_rest) xs