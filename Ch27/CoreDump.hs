module CoreDump where

discriminatory :: Bool -> Int
discriminatory b = 
    case b of
        False -> 0
        True -> 1

hypo'' :: IO ()
hypo'' = do
    let x :: Integer
        x = undefined
    s <- x `seq` getLine
    case s of
        "hi" -> print x
        _ -> putStrLn "hello"
