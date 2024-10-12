altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs) = f x : altMap g f xs

luhnDouble :: Int -> Int
luhnDouble x = if d > 9 then d - 9 else d where d = x * 2

luhn :: [Int] -> Bool
luhn = (==0) . (`mod` 10) . sum . altMap id luhnDouble . reverse

main :: IO ()
main = do
    print $ luhn [1, 7, 8, 4]
    print $ luhn [4, 7, 8, 3]
    print $ luhn [1, 7, 8, 4, 5, 4, 7, 8, 3]
