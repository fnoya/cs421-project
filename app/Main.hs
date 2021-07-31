module Main where
import Zurg
import IS

main :: IO ()
main = do
        putStrLn "search"
        putStrLn "------"
        print  (search (emptyM, (L, toys)))
        putStrLn ""
        putStrLn "searchIS"
        putStrLn "------"
        print (IS.finalize (searchIS (emptyM, (L, toys))))
        putStrLn ""
        putStrLn "searchD"
        putStrLn "------"
        print (IS.finalize (searchD (emptyM, (L, toys))))
        putStrLn ""
        return ()