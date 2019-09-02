tensDigit :: Integral a => a -> a
tensDigit x = d
    where   xLast = fst $ x `divMod` 10
            d = snd $ xLast `divMod` 10

foldBool3 :: a -> a -> Bool -> a
-- foldBool3 x y z =   case z of
                        -- False -> x
                        -- True -> y
foldBool3 x y z
    | z == False = x
    | z == True = y
