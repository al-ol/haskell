head':: [a] -> a
head' [] = undefined
head' (x:_) = x

tail':: [a] -> [a]
tail' [] = undefined
tail' (x:xs) = xs

take' :: Int -> [a] -> [a]
take n [] = undefined
take' 1 (x:xs)  = [x]
take' n (x:xs) = x:take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' n [] = []
drop' 1 (x:xs) = xs
drop' n (x:xs) = drop' (n-1) xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) = if f x == True then x: filter' f xs else filter' f xs

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f z [] = z
foldl' f z (x:xs) = foldl' f (f z x) xs

concat' :: [a] -> [a] -> [a]
concat' [] (x) = x
concat' (x:xs) (y) = x: concat' xs y

quickSort' :: Ord a => [a] -> [a]
quickSort' [] = []
quickSort' xs = concat' (concat' (quickSort' (filter' (< head' xs) xs)) (filter' (== head' xs) xs)) (quickSort' (filter' (> head' xs) xs))

