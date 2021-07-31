{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Zurg where
import GHC.OldList
import Prelude hiding (sum, maximum)
import Debug.Trace
import IS

type Space m s = [([m],s)]
class SearchProblem s m where
    trans :: s -> [(m,s)]
    isSolution :: ([m],s) -> Bool
    isGoal :: ([m],s) -> Bool
    space, solutions :: s -> Space m s

    cost :: ([m],s) -> Int

    search :: ([m],s) -> Int
    search (ms,state)
                | isGoal (ms,state) = cost (ms,state)
                | otherwise = GHC.OldList.minimum [search (moves : ms,ss) | (moves,ss) <- trans state ]

    searchIS ::  ([m],s) -> IS Int
    searchIS (ms,state)
                | isGoal (ms,state)= cost (ms,state) <? E
                | otherwise = cost (ms,state) <?
                        IS.minimum [searchIS (moves : ms, ss) | (moves,ss) <- trans state]

    searchD :: ([m],s) -> IS Int
    searchD (ms, state) =  searchD' (ms,state) (maxBound <? E)
        where
            searchD' (ms,state) u
                        | isGoal (ms,state) = cost (ms,state) <? E
                        | otherwise = cost (ms,state) <?
                                IS.minimumD u [searchD' (moves : ms, ss) | (moves,ss) <- trans state]

    space s = step ++ expand step
        where
            step = [ ([m],t) | (m,t) <- trans s ]
            expand ss = [ (ms++ns,t) | (ms,s) <- ss, (ns,t) <- space s ]

    solutions = filter isSolution . space

data Toy = Buzz | Hamm | Rex | Woody  | T1 -- | T2 -- | T3 | T4
    deriving (Eq,Ord,Show)
data Pos = L | R deriving (Eq,Show)
type Group = [Toy]
type BridgePos = (Pos,Group)
type Move = Either Toy Group

toys :: [Toy]
toys = [Buzz,Hamm,Rex,Woody,T1] --,T2] -- ,T3,T4]

time :: Toy -> Int
time Buzz = 5
time Woody = 10
time Rex = 20
time Hamm = 25
time T1 = 11
-- time T2 = 13
-- time T3 = 14
-- time T4 = 15

backw :: Group -> [(Move,BridgePos)]
backw xs = [(Left x,(L, sort (x:(toys \\ xs)))) | x <- xs]

forw :: Group -> [(Move,BridgePos)]
forw xs = [(Right [x,y],(R, delete y ys)) | x <- xs,let ys=delete x xs, y <- ys, x<y]

instance SearchProblem BridgePos Move where
    trans (L,l) = forw l
    trans (R,l) = backw (toys \\ l)
    isSolution (ms,s) = s == (R,[]) && trace (show (duration ms)) (duration ms <= 60) -- duration ms <= 60 -- 
    isGoal (ms,s) = s == (R,[])
    cost (ms, s) = duration ms

duration :: [Move] -> Int
duration = sum . map (either time (maximum.map time))

solution :: Space Move (Pos, [Toy])
solution = solutions (L,toys)

emptyM :: [Move]
emptyM = []

-- :set +s for timing in stack