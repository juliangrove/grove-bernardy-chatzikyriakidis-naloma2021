module Main where

import Fragment.RSA_Examples
import Representations.Bayesian.Bayesian

main :: IO ()
main = do
  putStrLn "example (enter 1 or 2):"
  ex <- getLine
  putStrLn "temperature:"
  alpha <- getLine
  putStrLn "noun phrase cost:"
  npC <- getLine
  putStrLn "pronoun cost:"
  pC <- getLine
  let ex' = read ex :: Int
      alpha' = read alpha :: Double
      npC' = read npC :: Double
      pC' = read pC :: Double
      result = show $ case ex' of
                        1 -> runRSA (ex1 alpha' npC' pC')
                        2 -> runRSA (ex2 alpha' npC' pC')
  putStrLn $ "ex" ++ ex
  putStrLn $ "----"
  putStrLn $ "temperature: " ++ alpha
  putStrLn $ "noun phrase cost: " ++ npC
  putStrLn $ "pronoun cost: " ++ pC
  putStrLn $ "result: " ++ result
