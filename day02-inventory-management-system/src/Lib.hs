module Lib
    ( processInput
    , checksum
    , partialDupe
    ) where

import Data.List
    ( transpose
    )
import Data.Set
    ( Set
    )
import qualified Data.Set as Set
import Control.Lens
    ( Lens'
    , lens
    , over
    )
import Control.Applicative
    ( (<|>)
    )
import Control.Arrow
    ( (&&&)
    , (***)
    )
import Control.Monad
   ( foldM
   )
import Control.Monad.State.Lazy
   ( State
   , runState
   , get
   , modify
   )

data CountSets a = CountSets
    { ones :: Set a
    , twos :: Set a
    , threes :: Set a
    , fourOrMores :: Set a
    }

empty :: CountSets a
empty = CountSets
    { ones = Set.empty
    , twos = Set.empty
    , threes = Set.empty
    , fourOrMores = Set.empty
    }

onesLens :: Lens' (CountSets a) (Set a)
onesLens = lens ones (\s b -> s { ones = b })
twosLens :: Lens' (CountSets a) (Set a)
twosLens = lens twos (\s b -> s { twos = b })
threesLens :: Lens' (CountSets a) (Set a)
threesLens = lens threes (\s b -> s { threes = b })
fourOrMoresLens :: Lens' (CountSets a) (Set a)
fourOrMoresLens = lens fourOrMores (\s b -> s { fourOrMores = b })

increment :: (Ord a) => a -> CountSets a -> CountSets a
increment a s
    | Set.member a (fourOrMores s) = s
    | Set.member a (threes s) = promote threesLens fourOrMoresLens a s
    | Set.member a (twos s) = promote twosLens threesLens a s
    | Set.member a (ones s) = promote onesLens twosLens a s
    | otherwise = over onesLens (Set.insert a) s
    where
        promote from to value = over to (Set.insert value) . over from (Set.delete value)

dup :: a -> (a, a)
dup a = (a, a)

pairWise :: (a -> b) -> (a, a) -> (b, b)
pairWise = uncurry (***) . dup

twosAndThrees :: (Ord a) => [a] -> (Bool, Bool)
twosAndThrees = pairWise (not . Set.null) . (twos &&& threes) . foldr increment empty

checksum :: (Ord a) => [[a]] -> Int
checksum = uncurry (*) . pairWise (length . filter id) . unzip . map twosAndThrees

findDupe :: (Ord a) => [a] -> Maybe a
findDupe = fst . (`runState` Set.empty) . foldM checkDupe Nothing
    where
        checkDupe :: (Ord a) => Maybe a -> a -> State (Set a) (Maybe a)
        checkDupe (Just v) a = return $ Just v
        checkDupe Nothing a = (fmap (Set.member a) $ get) >>= \found -> if found
            then return $ Just a
            else modify (Set.insert a) >> return Nothing

dropOptions :: [[a]] -> [[[a]]]
dropOptions = fmap transpose . drop1Choice . transpose
    where
        drop1Choice :: [a] -> [[a]]
        drop1Choice [] = []
        drop1Choice (x:xs) = xs:(map (x:) $ drop1Choice xs)

partialDupe :: Ord a => [[a]] -> Maybe [a]
partialDupe = foldr (<|>) Nothing . map findDupe . dropOptions

processInput :: (Show a) => ([String] -> a) -> IO ()
processInput f = getContents >>= return . f . lines >>= print
