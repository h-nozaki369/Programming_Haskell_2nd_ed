altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs) = f x : altMap g f xs

main :: IO ()
main = print $ altMap (+10) (+100) [0..4]
