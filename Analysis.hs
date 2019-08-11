{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS -Wno-name-shadowing #-}

module Analysis
  ( RunLog, RunLog_ (..)
  , DataSet, DataSet_ (..)
  , Trace
  , Analysis
  , GC(..)
  , Mode(..)
  , enumerate
  , parserFor
  , plotTrace
  , plotAnalysis
  ) where

import Control.Applicative
import Control.Monad
import Development.Shake hiding (Normal)
import Data.Char
import Data.Functor.Identity
import Data.List
import Data.Maybe
import Generics.Deriving
import qualified Graphics.Rendering.Chart.Easy as E
import Graphics.Rendering.Chart.Easy ((.=))
import qualified Graphics.Rendering.Chart.Backend.Diagrams as E
import System.FilePath
import Text.Read
import qualified Text.ParserCombinators.ReadP as P

-- | Various metrics of interest
data Metric = Runtimes | Pauses | MaxResidency
  deriving (Bounded, Generic, Read, Show)
  deriving (GEq, GEnum) via Default Metric

parserFor :: Metric -> String -> [Char]
parserFor Pauses = parsePause
parserFor Runtimes = parseRuntime
parserFor MaxResidency = parseMaxResidency

-- | Run mode - normal or twice as many iterations
data Mode = Normal | ExtraIterations
  deriving (Generic, Bounded, Read, Show)
  deriving (GEq, GEnum) via Default Mode

-- | Program to run
data Program = PusherBS | PusherDouble
  deriving (Generic, Bounded, Read, Show)
  deriving (GEq, GEnum) via Default Program

-- | Garbage collector to use
data GC = Copying | Incremental
  deriving (Generic, Bounded, Read, Show)
  deriving (GEq, GEnum) via Default GC

-- | A file path containing the output of -S for a given run
data RunLog_ f = RunLog
  { gc   :: HKD f GC
  , mode :: HKD f Mode
  , program :: HKD f Program
  , size :: HKD f Int
  }

type RunLog = RunLog_ Identity

instance Show RunLog where show RunLog{..} = show size <.> show mode <.> show program <.> show gc <.> "log"
instance Read RunLog where
  readPrec = do
    size <- readPrec <* dot
    mode <- readPrec <* dot
    program <- readPrec <* dot
    gc <- readPrec <* dot
    "log" <- many get
    return RunLog{..}

data TraceMetric = Allocated | Copied | Live | User | Elapsed
  deriving (Generic, Read, Show)
  deriving (GEq, GEnum) via Default TraceMetric

data Trace = Trace
  { traceMetric :: TraceMetric
  , runLog :: RunLog_ Over
  }
  deriving (Generic)

instance Show Trace where
  show Trace{traceMetric, runLog = RunLog{..}} =
    show traceMetric <.> show size <.> show mode <.> show program <.> show gc <.> "svg"

instance Read Trace where
  readPrec = do
    traceMetric <- readPrec <* dot
    size    <- (given <$> readPrec <* dot) <|> pure OverAll
    mode    <- (given <$> readPrec <* dot) <|> pure OverAll
    program <- (given <$> readPrec <* dot) <|> pure OverAll
    gc      <- (given <$> readPrec <* dot) <|> pure OverAll
    "svg"     <- many get
    let runLog = RunLog{..}
    return Trace{..}

toRunLogs :: Trace -> [RunLog]
toRunLogs Trace { runLog = RunLog {..} } =
  [ RunLog { .. }
  | gc      <- over gc
  , mode    <- over mode
  , size    <- over size
  , program <- over program
  ]

-- | A line in the output of -S
data Frame = Frame
  { allocated, copied, live :: Int
  , user, elapsed, totUser, totElapsed :: Double
  , generation :: Int
  }
  deriving (Show)

instance Read Frame where
  readPrec = do
    spaces
    allocated  <- readPrec <* spaces
    copied     <- readPrec <* spaces
    live       <- readPrec <* spaces
    user       <- readPrec <* spaces
    elapsed    <- readPrec <* spaces
    totUser    <- readPrec <* spaces
    totElapsed <- readPrec <* spaces
    _          <- readPrec @Int <* spaces
    _          <- readPrec @Int <* spaces
    "(Gen:  "  <- replicateM 7 get
    generation <- readPrec
    ')'        <- get
    return Frame { .. }
    where
      spaces = readP_to_Prec $ const P.skipSpaces

loadRunLog :: RunLog -> Action [Frame]
loadRunLog rl = do
  ll <- lines <$> readFile' (show rl)
  return $ mapMaybe readMaybe ll

-- | A set of metrics collected from multiple runs over varying data sizes
data DataSet_ f = DataSet
  { metric  :: HKD f Metric
  , gc      :: HKD f GC
  , mode    :: HKD f Mode
  , program :: HKD f Program
  }
  deriving (Generic)

deriving via Default DataSet  instance GEq DataSet
deriving via Default DataSet  instance GEnum DataSet
deriving via Default Analysis instance GEq Analysis
deriving via Default Analysis instance GEnum Analysis

type DataSet = DataSet_ Identity
type Analysis = DataSet_ Over

load :: DataSet -> Action [(Double, Double)]
load dataset = do
  ll <- lines <$> readFile' (show dataset)
  return [ (read a, read b) | [a,b] <- map words ll ]

instance Show DataSet where show DataSet{..} = show metric <.> show program <.> show mode <.> show gc <.> "dataset"
instance Read DataSet where
  readPrec = do
    metric <- readPrec <* dot
    program <- readPrec <* dot
    mode <- readPrec <* dot
    gc <- readPrec <* dot
    "dataset"<- many get
    return DataSet{..}

toDataSets :: Analysis -> [DataSet]
toDataSets DataSet{..} =
  [DataSet{..}
  | gc <- over gc
  , mode <- over mode
  , metric <- over metric
  , program <- over program
  ]

instance Show Analysis where
  show DataSet{..} = show metric <.> show program <.> show mode <.> show gc <.> "svg"

instance Read Analysis where
  readPrec = do
    metric  <- (given <$> readPrec <* dot) <|> pure OverAll
    program <- (given <$> readPrec <* dot) <|> pure OverAll
    gc      <- (given <$> readPrec <* dot) <|> pure OverAll
    mode    <- (given <$> readPrec <* dot) <|> pure OverAll
    "svg"     <- many get
    return DataSet{ .. }

-- -----------------------------------------------------------------------------------------------------------
-- Type Classes

class HasTitle a where title :: a -> String
instance HasTitle Metric where
  title Runtimes = "Runtime (s)"
  title Pauses = "Max Gen1 pause (s)"
  title MaxResidency = "Max Residency (bytes)"

instance HasTitle DataSet where
  title DataSet{..}= show metric <> "-" <> show program <> "-" <> show mode <> "-" <> show gc

instance HasTitle Analysis where
  title DataSet{..}   = intercalate " - " $ filter (not.null) [show metric, show program, show gc, show mode]

instance HasTitle Trace where
  title Trace { runLog = RunLog {..}, ..} = intercalate " - "
    $ filter (not . null) [show traceMetric, show program, show size, show gc, show mode]
-- ------------------------------------------------------------------------------------------------------------
-- Functions

parsePause :: String -> [Char]
parsePause input =
    case find ("  Gen  1" `isPrefixOf`) (lines input) of
      Just l -> init (last $ words l)
      Nothing -> ""

parseRuntime :: String -> String
parseRuntime input =
  case find ("  Total   time" `isPrefixOf`) (lines input) of
    Just l -> (init $ head $ tail $ reverse $ words l)
    Nothing -> ""

parseMaxResidency :: String -> String
parseMaxResidency input =
  case find ("maximum residency" `isInfixOf`) (lines input) of
    Just l -> filter isDigit $ head (words l)
    Nothing -> ""

enumerate :: forall a . (GEnum a) => [a]
enumerate = genum

dot :: ReadPrec ()
dot = get >>= guard . (== '.')

plotAnalysis :: Analysis -> FilePath -> Action ()
plotAnalysis analysis out = do
  let datasets = toDataSets analysis
  datas <- mapM load datasets
  liftIO $ E.toFile E.def out $ do
    E.layout_title .= title analysis
    forM_ (zip datasets datas) $ \(d,dat) ->
      E.plot (E.line (labelDataSetInAnalysis analysis d) [dat])

labelDataSetInAnalysis :: Analysis -> DataSet -> String
labelDataSetInAnalysis an DataSet{..} = intercalate " - " $
  filter (not.null) $
  [ show metric | OverAll <- [case an of DataSet{..} -> metric]] ++
  [ show program | OverAll <- [case an of DataSet{..} -> program]] ++
  [ show gc | OverAll <- [case an of DataSet{..} -> gc]] ++
  [ show mode | OverAll <- case an of DataSet{..} -> [mode]]

plotTrace :: Trace -> FilePath -> Action ()
plotTrace t@Trace { traceMetric } out = do
  let runLogs = toRunLogs t
      extract = frameMetric traceMetric
  frames <- mapM loadRunLog runLogs
  liftIO $ E.toFile E.def out $ do
    E.layout_title .= title t
    forM_ (zip runLogs frames) $ \(rl, ff) -> E.plot
      (E.line
        (labelRunLogInTrace t rl)
        [zipWith (\i f -> (i, extract f)) [(0 :: Double) ..] ff]
      )
  return ()

labelRunLogInTrace :: Trace -> RunLog -> String
labelRunLogInTrace trace RunLog{..} = intercalate " - " $
  filter (not.null) $
  [ show size | OverAll <- [case runLog trace of RunLog{..} -> size]] ++
  [ show program | OverAll <- [case runLog trace of RunLog{..} -> program]] ++
  [ show gc | OverAll <- [case runLog trace of RunLog{..} -> gc]] ++
  [ show mode | OverAll <- case runLog trace of RunLog{..} -> [mode]]

frameMetric :: TraceMetric -> Frame -> Double
frameMetric Allocated = fromIntegral . allocated
frameMetric Copied = fromIntegral . copied
frameMetric Live = fromIntegral . live
frameMetric Elapsed = elapsed
frameMetric User = user

----------------------------------------------------
-- Dual purpose types for data and analysis

type family HKD (f :: * -> *) a
type instance HKD Identity a = a
type instance HKD Over a = Over a

data Over a = Over [a] | OverAll
  deriving (Generic)
  deriving (GEq, GEnum) via Default (Over a)

instance Show a => Show (Over a) where
  show OverAll = ""
  show (Over xx) = intercalate "&&" $ map show xx

given :: a -> Over a
given x = Over [x]

over :: GEnum a => Over a -> [a]
over (Over xx) = xx
over OverAll   = enumerate
