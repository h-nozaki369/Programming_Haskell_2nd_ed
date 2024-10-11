luhnDouble :: Int -> Int
luhnDouble x = if d > 9 then d - 9 else d where d = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z w = (luhnDouble x + y + luhnDouble z + w) `mod` 10 == 0

main :: IO ()
main = do
    print $ luhn 1 7 8 4
    print $ luhn 4 7 8 3
