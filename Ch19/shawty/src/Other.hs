module Other where

import Debug.Trace

d f = trace ("{" ++ show f ++ "}") f

quickSort [] = []
quickSort (x:xs) = quickSort ltx ++ [x] ++ quickSort gtx
    where
        ltx = [a | a <- xs, a <= x]
        gtx = [a | a <- xs, a > x]

quickSortWithLog [] = []
quickSortWithLog (x:xs) = quickSortWithLog (d ltx) ++ [x] ++ quickSortWithLog gtx
    where
        ltx = [a | a <- xs, a <= x]
        gtx = [a | a <- xs, a > x]

