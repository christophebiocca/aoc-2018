module Lib
    ( process
    , countOverlap
    , findNonOverlapping
    )
    where

import Control.Arrow
    ( left
    , right
    )
import Control.Monad
    ( liftM
    )
import Data.List
    ( find
    )
import Data.Set
    ( Set
    )
import qualified Data.Set as Set
import Data.Void
    ( Void
    )
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
    ( char
    , string
    )
import Text.Megaparsec.Char.Lexer
    ( decimal
    )

type Parser = MP.Parsec Void String

data Claim = Claim
    { index :: Integer
    , x :: Integer
    , y :: Integer
    , width :: Integer
    , height :: Integer
    }
    deriving (Show)

data Point = Point Integer Integer deriving (Show, Eq, Ord)

intoPoints :: Claim -> [Point]
intoPoints claim = Point <$> map (+ (x claim)) [0..(width claim - 1)] <*> map (+ (y claim)) [0..(height claim - 1)]

lineParser :: Parser Claim
lineParser = do
    char '#'
    index <- decimal
    string " @ "
    x <- decimal
    char ','
    y <- decimal
    string ": "
    width <- decimal
    char 'x'
    height <- decimal
    return Claim
        { index = index
        , x = x
        , y = y
        , width = width
        , height = height
        }

fileParser :: Parser [Claim]
fileParser = MP.endBy lineParser (char '\n')

parseInput :: IO (Either String [Claim])
parseInput = liftM (left MP.parseErrorPretty . MP.parse fileParser "input") getContents

data ClaimOverlap = ClaimOverlap
    { claimedOnce :: Set Point
    , overlapping :: Set Point
    }

empty :: ClaimOverlap
empty = ClaimOverlap Set.empty Set.empty

addClaim :: Claim -> ClaimOverlap -> ClaimOverlap
addClaim = (flip $ foldr addPoint) . intoPoints
    where
        addPoint :: Point -> ClaimOverlap -> ClaimOverlap
        addPoint p co@ClaimOverlap{claimedOnce=claimedOnce, overlapping=overlapping}
            | Set.member p overlapping = co
            | Set.member p claimedOnce = ClaimOverlap {overlapping=Set.insert p overlapping, claimedOnce=Set.delete p claimedOnce}
            | otherwise = co{claimedOnce=Set.insert p claimedOnce}

overlapArea = Set.size . overlapping

hasOverlap :: ClaimOverlap -> Claim -> Bool
hasOverlap co c = any (`Set.member` (overlapping co)) $ intoPoints c

computeOverlaps :: [Claim] -> ClaimOverlap
computeOverlaps = foldr addClaim empty

countOverlap :: [Claim] -> Int
countOverlap = overlapArea . computeOverlaps

findNonOverlapping :: [Claim] -> Maybe Integer
findNonOverlapping cs = fmap index $ find (not . hasOverlap (computeOverlaps cs)) cs

process :: Show a => ([Claim] -> a) -> IO ()
process f = parseInput >>= (return . right f) >>= print
