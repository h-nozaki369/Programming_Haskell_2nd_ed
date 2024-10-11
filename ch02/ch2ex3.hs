-- 1. N should be small letter.
-- 2. div should be srounded by ` `
-- 3. lenght xs needs parentheses. 
n = a `div` (length xs)
    where
        a = 10
        xs = [1,2,3,4,5]

main :: IO ()
main = print n
