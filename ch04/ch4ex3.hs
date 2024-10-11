safetail1, safetail2, safetail3 :: [a] -> [a]

safetail1 xs = if null xs then [] else tail xs

safetail2 xs | null xs   = []
             | otherwise = tail xs

safetail3 [] = []
safetail3 xs = tail xs

main :: IO ()
main = do
    print (safetail1 [] :: [Int])
    print (safetail2 [] :: [Int])
    print (safetail3 [] :: [Int])
    print $ safetail1 [1..5]
    print $ safetail2 [1..5]
    print $ safetail3 [1..5]
