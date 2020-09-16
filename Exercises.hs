{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR COURSEWORK 1 for COMP2209, 2019
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2019

module Exercises (histogram,approxSqrt,longestCommonSubsequence,neighbours,findBonding,insertFromCurrentNode,VTree(..),Direction(..),Trail(..),Zipper(..),Instruction(..),Stack,SMProg,evalInst,findMaxReducers,optimalPower) where

-- The following two imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Data.Char
import Data.List 
import Data.Foldable
import Data.Function 
import Control.Monad
import Data.Function (on)
import Data.List (sortBy)
import Data.Ord





-- Exercise A1
histogram :: Int -> [Int] -> [Int]
histogram a xs 
     | a < 0 = error "Negative input"
     |(a == 0) =  error "Input is 0"
     |otherwise = [numTimesFound y (sortDivide a xs) | y <- [0..max]]
    where max = Data.List.maximum(sortDivide a xs)

sortDivide :: Int -> [Int] -> [Int]
sortDivide b zs = sort([ z `div` b| z <- zs])

numTimesFound :: Ord a => a -> [a] -> Int
numTimesFound _ [] = 0
numTimesFound x xs = (length . filter (== x)) xs





-- Exercise A2
approxSqrt :: Double -> Double -> Double
approxSqrt n e
   | n > 0 && e>0  = babylonSqrt n 1
   | n == 0 && e>0 = 0
   | n < 0 && e>0  = error "Negative input"
   |otherwise = error "Epsilon value must be positive"
   where
   babylonSqrt d x    | abs (sqrt(d)-x) >= e = babylonSqrt d y
                      | abs (sqrt(d)-x) < e  = x
                          where y = (x + d/x)/ 2





-- Exercise A3
longestCommonSubsequence :: Eq a => [[a]] -> [a]
longestCommonSubsequence [] = []
longestCommonSubsequence xs = Data.Foldable.foldl1 intersection' xs
                                 where intersection' xs ys = xs \\ (xs \\ ys)
                                 
                                 
                                 
                                 

-- Exercise A4
metric :: Point a -> Point a -> Double
metric (a,b) (c,d) = ((c-a)^2 + (d-b)^2) ** (0.5)

type Point a = (Double, Double)
type Metric a = Point a -> Point a -> Double

neighbours :: Int -> Metric a -> Point a -> [Point a] -> [(Point a)] 
neighbours k f p xs = take k list
    where list = map (fst) ( sortBy (compare `on` snd) [(x, f p x) | x <- xs])





-- Exercise A5
findBonding :: Eq a => (a -> a -> Bool) -> [a] -> Maybe [(a,a)]
findBonding f [] = Just []
findBonding f xs   
                       | ((length(xs) `mod` 2) == 0) = findAllLists (reverse (getSymmetricBondings (createTuples1 f xs))) xs 1
                       | otherwise = Nothing 

createTuples1 func xs = [(x,y) | x <- xs, y <- xs, func x y == True, x /= y]

getSymmetricBondings xs = [ x | x<- xs, (snd(x), fst(x)) `Data.List.elem` xs]


createSymmetricBondings xs = findBonding1 xs ++ [(y,x)|(x,y) <- findBonding1 xs]


findBonding1 [] = []
findBonding1 xs = ([(head(xs))] ++ findBonding1[x | x <- tail(xs), fst(x) /= fst(head(xs)), snd(x) /= fst(head(xs)), fst(x) /= snd(head(xs)), snd(x) /= snd(head(xs))])


findAllLists xs ys a   | a + 1 > length xs = Nothing
                       | (length b) * 2 == length ys = Just (findBonding2 b)
                       | otherwise = findAllLists ((tail(xs)) ++ [head xs]) ys (a+1)
                         where b = findBonding1 xs 

findBonding2 xs =  xs ++ [(y,x)| (x,y) <- xs]





-- Exercise A6
data VTree a = Leaf | Node (VTree a) a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
data Direction a = L a Int (VTree a) | R a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
type Trail a = [Direction a]
type Zipper a = (VTree a, Trail a)

instance NFData a => NFData (VTree a)
instance NFData a => NFData (Direction a)

insertFromCurrentNode :: Ord a => a -> Zipper a -> Zipper a
insertFromCurrentNode v z = (Leaf,[])





-- Exercise A7
data Instruction = Add | Mul | Dup | Pop deriving (Eq,Ord,Show,Generic)
type Stack = [Int]
type SMProg = [Instruction] 

instance NFData (Instruction)

evalInst :: Stack -> SMProg -> Stack
evalInst [] (y:ys) = error "Empty List"
evalInst xs [] = xs
evalInst xs (Add:ys)  | (length(xs) >= 2) = evalInst (([xs !! 0  + xs !! 1] ++ xs) \\ [xs !! 1, xs !! 0]) ys
                      | otherwise = error "Empty"
evalInst xs (Mul:ys)  | (length(xs) >= 2) = evalInst (([xs !! 0  * xs !! 1] ++ xs) \\ [xs !! 1, xs !! 0]) ys
                      | otherwise = error "Empty"
evalInst xs (Dup:ys) = evalInst ([xs !! 0] ++ xs) ys
evalInst xs (Pop:ys) = evalInst (xs \\ [xs !! 0]) ys 





-- Exercise A8

findMaxReducers :: Stack -> [SMProg]

combos n = replicateM (n-1) [Add, Mul, Pop] 

createTuples xs = (zip (combos (length(xs))) (zss))
  where zss = [evalInst xs ys| ys <- combos (length(xs))]

findMax xs = Data.List.maximumBy (comparing snd) (createTuples xs)

--findMaxReducers1 (x:xs) = maxTail (createTuples(x:xs)) (findMax (x:xs)) [findMax (x:xs)]

findMaxReducers1 (x:xs) = maxTail (createTuples(x:xs)) (findMax (x:xs)) [findMax (x:xs)]

maxTail [] z ys = ys
maxTail (x:xs) z ys | (snd(x) < snd(z)) = maxTail xs z ys
                    | (fst(x) /= fst(z)) && (snd(x) == snd(z)) = maxTail xs z ys++[x]
                    | otherwise = maxTail xs z ys

findMaxReducers xs | length xs > 0 = [fst(x) | x <- findMaxReducers1 (xs)]
                   | length xs == 0 = []





-- Exercise A9

optimalPower :: Int -> SMProg
optimalPower 1 = []
optimalPower n = if (n <= 0) then error "Incorrect power"
                 else if (isPrime(n)) then ([Dup] ++ optimalPower(n-1) ++ [Mul])
                 else if (isOdd(n)) then (findOptimal n)
                 else (findOptimal1 n)

isOdd :: Int -> Bool
isOdd n = if((n  `mod` 2) == 0) then False else True

findOptimal :: Int -> SMProg
findOptimal n = if (length(nMinus1) < length (findOptimal1 n)) then nMinus1
                else (findOptimal1 n)
                  where nMinus1 = [Dup] ++ (findOptimal1(n-1)) ++ [Mul]

findOptimal1 :: Int -> SMProg
findOptimal1 n = head[snd x | x <- xs, fst x == (getMin xs)]
                     where xs = findInstructions (generatePairsOfFactors n)

getMin :: [(Int, SMProg)] -> Int
getMin zs = Data.List.minimum [fst z | z <- zs]

generatePairsOfFactors :: Int -> [(Int, Int)]
generatePairsOfFactors n = deleteSamePairs [(x, (n `div` x)) | x <- [2 .. n-1], mod n x == 0]

deleteSamePairs :: [(Int, Int)] -> [(Int, Int)]
deleteSamePairs [] = [] 
deleteSamePairs (x:xs) = [x] ++ deleteSamePairs ([(n,m) | (n,m) <- xs, ( ((fst x) > m) || ((fst x) < m) ) && ( ((snd x) > n) || ((snd x) < n)) ])

findInstructions :: [(Int, Int)] -> [(Int, SMProg)]
findInstructions pairs = [(length xs, xs)| ys <- pairs, xs <- [(optimalPower (fst ys)) ++ (optimalPower (snd ys))]]

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n | ((length [x | x <- [2 .. n-1], mod n x == 0]) > 0) = False
          | otherwise = True
