scanlMy :: (a -> b -> a) -> a -> [b] -> [a]
scanlMy f q ls = 
    q : (case ls of
            [] -> []
            x:xs -> scanlMy f (f q x) xs)

fibs :: [Int]
fibs = 1 : scanl (+) 1 fibs
-- fibs = show 1 : scanl (\y x -> concat ["(",x,"+",y,")"]) (show 1) fibs

fibsN x = fibs !! x

fibs20 = take 20 fibs

fibsLessThan x = takeWhile (< x) fibs

-- fibsF = scanl (+) 1 fibsF
-- fibsF = scanl (\y x -> concat ["(",x,"+",y,")"]) (show 1) fibsF

factorial :: [Int]
factorial = scanl (*) 1 [1..] 

--

stops = "pbtdkg"

vowels = "aeiou"

tuple3 = (,,) <$> stops <*> vowels <*> stops
-- tuple3 = [ (x,y,z) | x <- stops, y <- vowels, z <- stops ]

-- tuple3StartWithP = filter (\(x,_,_) -> x == 'p') tuple3
tuple3StartWithP = [ ('p',y,z) | y <-vowels, z <- stops ]

--

seekritFunc x =
    div (sum (map length (words x)))
        (length (words x))
-- seekritFunc x =
    -- (fromRational (fromIntegral (sum (map length (words x))))) /
        -- (fromRational (fromIntegral (length (words x))))
    
