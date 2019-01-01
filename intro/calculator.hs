-- Chapter 1 Exercise 2
module Main where
import System.Environment

main :: IO ()
main = do 
  args <- getArgs
  let x = read(args !! 0) :: Float
  let y = read(args !! 1) :: Float
  putStrLn $ show $ x + y
