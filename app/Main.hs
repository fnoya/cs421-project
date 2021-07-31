module Main where
import Zurg
import IS
import System.TimeIt

main :: IO ()
main = do
        putStrLn "search"
        putStrLn "------"
        timeIt $ print  (search (emptyM, (L, toys)))
        putStrLn ""
        putStrLn "searchIS"
        putStrLn "------"
        timeIt $ print (IS.finalize (searchIS (emptyM, (L, toys))))
        putStrLn ""
        putStrLn "searchD"
        putStrLn "------"
        timeIt $ print (IS.finalize (searchD (emptyM, (L, toys))))
        putStrLn ""
        return ()