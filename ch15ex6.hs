sqroot :: Double -> Double
sqroot n = snd . head $ dropWhile (\(x,y) -> abs(x-y) > epsilon) $ zip list (tail list) 
           where list = candidates n

candidate :: Double = 0.1
epsilon :: Double = 0.00001

candidates :: Double -> [Double]
candidates n = iterate next candidate
               where
                   next a = (a + n / a) / 2
