module Lib
    (
        sumFrequencies,
        dupeFrequencies
    ) where

import Data.Maybe(fromJust)
import Data.List(find)
import Data.Set(Set)
import qualified Data.Set as Set
import Control.Arrow
    (
        (&&&)
    )

sumChanges :: [Integer] -> Integer
sumChanges = foldr (+) 0

parseChange :: String -> Integer
parseChange ('-':s) = -(read s)
parseChange ('+':s) = read s

runningSum :: [Integer] -> [Integer]
runningSum = scanl (+) 0

runningSet :: [Integer] -> [Set Integer]
runningSet = scanl (flip Set.insert) Set.empty

firstRepeat :: [Integer] -> Integer
firstRepeat = fst . fromJust . find snd . map (fst &&& uncurry Set.member) . uncurry zip . (tail &&& runningSet) . runningSum . cycle

solve :: Show r => ([Integer] -> r) -> IO ()
solve f = getContents >>= return . f . map parseChange . lines >>= print

sumFrequencies :: IO ()
sumFrequencies = solve sumChanges

dupeFrequencies :: IO ()
dupeFrequencies = solve firstRepeat
