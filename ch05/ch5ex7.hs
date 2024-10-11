a = [(x,y) | x <- [1,2,3], y <- [4,5,6]]
b = concat [ [(x,y) | y <- [4,5,6]] | x <- [1,2,3]]

main :: IO ()
main = do
    print a
    print b
