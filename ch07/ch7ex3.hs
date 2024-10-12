map :: (a -> b -> b) -> [a] -> [b]
map f xs = foldr (\x ys -> f x : ys) [] xs

filter :: (a -> Bool) -> [a] -> [a]
filter p xs = foldr (\x xs' -> if p x then x : xs' else xs') [] xs
