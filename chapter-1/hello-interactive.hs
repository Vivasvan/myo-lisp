-- Chapter 1 initial code blurb
module Main where
    import System.Environment
    
    main :: IO ()
    msg n = "Hello, " ++ n
    hello = putStrLn . msg
    main = getLine >>= hello