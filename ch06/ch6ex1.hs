fac :: Int -> Int
fac 0     = 1
fac n = if n < 0 then
            error "must be non-negative value"
        else
            n * fac (n-1)

main :: IO ()
main = do
    print $ fac 3
    print $ fac 0
    print $ fac (-2)
