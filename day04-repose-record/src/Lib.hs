module Lib
    ( process
    , sleepiestMinuteForSleepiestGuard
    , sleepiestMinuteGuardCombo
    ) where
import Control.Arrow
    ( left
    , (+++)
    )
import Control.Monad
    ( liftM2
    , (>=>)
    )
import Data.List
    ( sortOn
    , maximumBy
    )
import Data.Map
    ( fromListWith
    , toList
    )
import Data.Ord
    ( comparing
    )
import Data.Void
    ( Void
    )
import Text.Megaparsec
    ( Stream
    , Parsec
    , between
    , (<|>)
    , parseErrorPretty
    , parse
    , endBy
    )
import Text.Megaparsec.Char
    ( char
    , string
    )
import Text.Megaparsec.Char.Lexer
    ( decimal
    )

type Parser = Parsec Void String
type LogParser = Parsec Void [LogLine]

data GuardId = GuardId Integer deriving (Show, Eq, Ord)

data LogContent
    = ShiftStart GuardId
    | NapStart
    | NapEnd
    deriving (Show)

data LogLine = LogLine DateTime LogContent deriving (Show)

date :: LogLine -> DateTime
date (LogLine dt _) = dt

isShiftStart :: LogLine -> Bool
isShiftStart (LogLine _ (ShiftStart _)) = True
isShiftStart _ = False

data DateTime = DateTime
    { year :: Integer
    , month :: Integer
    , day :: Integer
    , hour :: Integer
    , minute :: Integer
    }
    deriving (Show, Eq, Ord)

-- Paired-up nap start and nap end
data Nap = Nap Integer Integer deriving (Show)

duration :: Nap -> Integer
duration (Nap start end) = end - start

minutes :: Nap -> [Integer]
minutes (Nap start end) = [start..(end - 1)]

-- Fully grouped
data Shift = Shift GuardId [Nap] deriving (Show)

hasId :: GuardId -> Shift -> Bool
hasId target (Shift gid _) = target == gid

totalNapsDuration :: Shift -> Integer
totalNapsDuration (Shift _ naps) = sum $ map duration $ naps

napMinutes :: Shift -> [Integer]
napMinutes (Shift _ naps) = concatMap minutes naps

dateParser :: Parser DateTime
dateParser = between (char '[') (char ']') dateContent
    where
        dateContent = do
            year <- decimal
            char '-'
            month <- decimal
            char '-'
            day <- decimal
            char ' '
            hour <- decimal
            char ':'
            minute <- decimal
            return DateTime
                { year=year
                , month=month
                , day=day
                , hour=hour
                , minute=minute
                }

logContentParser :: Parser LogContent
logContentParser = guardId <|> napStart <|> napEnd
    where
        guardId :: Parser LogContent
        guardId = fmap (ShiftStart . GuardId) $ between (string "Guard #") (string " begins shift") decimal
        napStart :: Parser LogContent
        napStart = string "falls asleep" >> return NapStart
        napEnd :: Parser LogContent
        napEnd = string "wakes up" >> return NapEnd

lineParser :: Parser LogLine
lineParser = liftM2 LogLine dateParser (char ' ' >> logContentParser)

fileParser :: Parser [LogLine]
fileParser = endBy lineParser (char '\n')

toShifts :: [LogLine] -> Either String [Shift]
toShifts [] = Right []
toShifts ((LogLine _ (ShiftStart guardId)):cs) = liftM2 (:) (fmap (Shift guardId) (pairUp naps)) (toShifts remainder)
    where
        (naps, remainder) = break isShiftStart cs
        pairUp :: [LogLine] -> Either String [Nap]
        pairUp [] = Right []
        pairUp ((LogLine ds NapStart):(LogLine de NapEnd):lls) = fmap ((Nap (minute ds) (minute de)):) (pairUp lls)
        pairUp _ = Left "Expected a nap start and end in that order"
toShifts _ = Left "Expected a shift id at the start of the range"

parseInput :: IO (Either String [Shift])
parseInput = fmap (((parseErrorPretty +++ sortOn date) . parse fileParser "input") >=> toShifts) getContents

sleepiestGuard :: [Shift] -> GuardId
sleepiestGuard = fst . maximumBy (comparing snd) . toList . fromListWith (+) . map (\s@(Shift gid _) -> (gid, totalNapsDuration s))

sleepiestMinute :: GuardId -> [Shift] -> Integer
sleepiestMinute guardId = fst . maximumBy (comparing snd) . toList . fromListWith (+) . map (\m -> (m, 1)) . concatMap napMinutes . filter (hasId guardId)

sleepiestMinuteForSleepiestGuard :: [Shift] -> (GuardId, Integer)
sleepiestMinuteForSleepiestGuard shifts = (guard, sleepiestMinute guard shifts)
    where
        guard = sleepiestGuard shifts

sleepiestMinuteGuardCombo :: [Shift] -> (GuardId, Integer)
sleepiestMinuteGuardCombo = fst . maximumBy (comparing snd) . toList . fromListWith (+) . map (\m -> (m, 1)) . concatMap (\s@(Shift gid _) -> [(gid, m) | m <- napMinutes s])

process :: Show a => ([Shift] -> a) -> IO ()
process f = parseInput >>= (return . fmap f) >>= print
