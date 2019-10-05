{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving #-}
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
  , GC, GC_(..)
  , Mode(..)
  , MsgTypeSing(..)
  , enumerate
  , parserFor
  , plotTrace
  , plotAnalysis
  ) where

import Control.Applicative
import Control.Monad
import Development.Shake hiding (Normal, (*>))
import Data.Char
import Data.Functor.Identity
import Data.List
import Data.Maybe
import Generics.Deriving
import qualified Graphics.Rendering.Chart.Easy as E
import Graphics.Rendering.Chart.Easy ((.=))
import qualified Graphics.Rendering.Chart.Backend.Diagrams as E
import Numeric.Natural
import Pusher (MsgTypeSing(..))
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

newtype Capabilities = Capabilities Natural
  deriving Generic
  deriving newtype (Eq, Num, Read, Show)
  deriving anyclass GEq

instance GEnum Capabilities where genum = [1,2,4]

newtype PauseMs = PauseMs Natural
  deriving Generic
  deriving newtype (Eq, Num, Read, Show)
  deriving anyclass GEq

instance GEnum PauseMs where genum = [1,10]

newtype IterBetweenPauses = IterBetweenPauses Natural
  deriving Generic
  deriving newtype (Eq, Num, Read, Show)
  deriving anyclass GEq

instance GEnum IterBetweenPauses where genum = [100,10000]

-- | Garbage collector to use
data GC_ f
  = Copying
  | Incremental (HKD f Capabilities)
  | IncrementalWithPauses (HKD f Capabilities) (HKD f PauseMs) (HKD f IterBetweenPauses)
  deriving (Generic)

type GC = GC_ Identity
type GCs = GC_ Over

deriving anyclass instance GEq GC
deriving anyclass instance GEnum GC
deriving anyclass instance GEq GCs
deriving anyclass instance GEnum GCs

flattenGCs :: GCs -> [GC]
flattenGCs Copying = pure Copying
flattenGCs (Incremental cap) = Incremental <$> over cap
flattenGCs (IncrementalWithPauses cap len iter) =
  IncrementalWithPauses <$> over cap <*> over len <*> over iter

instance Show GC where
  show Copying = "Copying"
  show (Incremental 1) = "Incremental"
  show (Incremental n) = "Incremental-" <> show n
  show (IncrementalWithPauses c ms n) = "IncrementalWithPauses-" <> show c <> "-" <> show ms <> "-" <> show n

instance Show (GCs) where
  show Copying = "Copying"
  show (Incremental (Over [1])) = "Incremental"
  show (Incremental n) = "Incremental-" <> show n
  show (IncrementalWithPauses c ms n) = "IncrementalWithPauses-" <> show c <> "-" <> show ms <> "-" <> show n

instance Read GC where
  readPrec = choice
    [ Copying <$ string "Copying"
    , Incremental 1 <$ string "Incremental"
    , Incremental <$> (string "Incremental-" *> readPrec)
    , do
        string "IncrementalWithPauses-"
        c <- readPrec
        char '-'
        ms <- readPrec
        char '-'
        n <- readPrec
        return (IncrementalWithPauses c ms n)
    ]

instance Read GCs where
  readPrec = choice
    [ Copying <$ string "Copying"
    , Incremental (Over [1]) <$ string "Incremental"
    , Incremental <$> (string "Incremental-" *> readPrec)
    , do -- TODO review
        string "IncrementalWithPauses"
        c  <- (char '-' *> readPrec) <|> pure OverAll
        ms <- (char '-' *> readPrec) <|> pure OverAll
        n  <- (char '-' *> readPrec) <|> pure OverAll
        return (IncrementalWithPauses c ms n)
    ]

-- | A file path containing the output of -S for a given run
data RunLog_ f = RunLog
  { gc   :: HKD f (GC_ f)
  , mode :: HKD f Mode
  , msgType :: HKD f MsgTypeSing
  , size :: HKD f Int
  }

type RunLog = RunLog_ Identity

instance Show RunLog where show RunLog{..} = show size <.> show mode <.> show msgType <.> show gc <.> "log"
instance Read RunLog where
  readPrec = do
    size    <- readPrec <* dot
    mode    <- readPrec <* dot
    msgType <- readPrec <* dot
    gc      <- readPrec <* dot
    "log"   <- many get
    return RunLog { .. }

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
    show traceMetric <.> show size <.> show mode <.> show msgType <.> show gc <.> "svg"

instance Read Trace where
  readPrec = do
    traceMetric <- readPrec <* dot
    size    <- (readPrec <* dot) <|> pure OverAll
    mode    <- (readPrec <* dot) <|> pure OverAll
    msgType <- (readPrec <* dot) <|> pure OverAll
    gc      <- (readPrec <* dot) <|> pure OverAll
    "svg"     <- many get
    let runLog = RunLog{..}
    return Trace{..}

toRunLogs :: Trace -> [RunLog]
toRunLogs Trace { runLog = RunLog {..} } =
  [ RunLog { .. }
  | gcs     <- over gc
  , gc      <- flattenGCs gcs
  , mode    <- over mode
  , size    <- over size
  , msgType <- over msgType
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
  , gc      :: HKD f (GC_ f)
  , mode    :: HKD f Mode
  , msgType :: HKD f MsgTypeSing
  }
  deriving (Generic)

deriving via Default DataSet  instance GEq DataSet
deriving via Default DataSet  instance GEnum DataSet
deriving via Default Analysis instance GEq Analysis
deriving via Default Analysis instance GEnum Analysis
deriving via Default MsgTypeSing instance GEq MsgTypeSing
deriving via Default MsgTypeSing instance GEnum MsgTypeSing

type DataSet = DataSet_ Identity
type Analysis = DataSet_ Over

load :: DataSet -> Action [(Double, Double)]
load dataset = do
  ll <- lines <$> readFile' (show dataset)
  return [ (read a, read b) | [a,b] <- map words ll ]

instance Show DataSet where show DataSet{..} = show metric <.> show msgType <.> show mode <.> show gc <.> "dataset"
instance Read DataSet where
  readPrec = do
    metric    <- readPrec <* dot
    msgType   <- readPrec <* dot
    mode      <- readPrec <* dot
    gc        <- readPrec <* dot
    "dataset" <- many get
    return DataSet { .. }

toDataSets :: Analysis -> [DataSet]
toDataSets DataSet{..} =
  [DataSet{..}
  | gc <- over gc >>= flattenGCs
  , mode <- over mode
  , metric <- over metric
  , msgType <- over msgType
  ]

instance Show Analysis where
  show DataSet{..} = show metric <.> show msgType <.> show mode <.> show gc <.> "svg"

instance Read Analysis where
  readPrec = do
    metric  <- (readPrec <* dot) <|> pure OverAll
    msgType <- (readPrec <* dot) <|> pure OverAll
    mode    <- (readPrec <* dot) <|> pure OverAll
    gc      <- (readPrec <* dot) <|> pure OverAll
    "svg"   <- many get
    return DataSet{ .. }

-- -----------------------------------------------------------------------------------------------------------
-- Figure titles

class HasTitle a where title :: a -> String
instance HasTitle Metric where
  title Runtimes = "Runtime (s)"
  title Pauses = "Max Gen1 pause (s)"
  title MaxResidency = "Max Residency (bytes)"

instance HasTitle DataSet where
  title DataSet{..}= show metric <> "-" <> title msgType <> "-" <> show mode <> "-" <> show gc

instance HasTitle Analysis where
  title DataSet{..}   = intercalate " - " $ filter (not.null) [show metric, title msgType, show gc, show mode]

instance HasTitle Trace where
  title Trace { runLog = RunLog {..}, ..} = intercalate " - "
    $ filter (not . null) [show traceMetric, title msgType, show size, show gc, show mode]

instance HasTitle MsgTypeSing where
  title B = "ByteStrings"
  title D = "Doubles"
  title S = "ShortByteStrings"

instance HasTitle a => HasTitle (Over a) where
  title OverAll = ""
  title (Over xx) = intercalate "&&" $ map title xx

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
  [ show metric | case an of DataSet{..} -> multi (over metric)] ++
  [ show msgType | case an of DataSet{..} -> multi (over msgType)] ++
  [ show gc | case an of DataSet{..} -> multi (over gc >>= flattenGCs)] ++
  [ show mode | case an of DataSet{..} -> multi (over mode)]

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
  [ show size | case runLog trace of RunLog{..} -> multi (over size)] ++
  [ show msgType | case runLog trace of RunLog{..} -> multi (over msgType)] ++
  [ show gc | case runLog trace of RunLog{..} -> multi (over gc >>= flattenGCs)] ++
  [ show mode | case runLog trace of RunLog{..} -> multi (over mode)]

frameMetric :: TraceMetric -> Frame -> Double
frameMetric Allocated = fromIntegral . allocated
frameMetric Copied = fromIntegral . copied
frameMetric Live = fromIntegral . live
frameMetric Elapsed = elapsed
frameMetric User = user

----------------------------------------------------
-- Dual purpose types for data and analysis

type family HKD (f :: * -> *) a where
  HKD Identity a = a
  HKD f a = f a

data Over a = Over [a] | OverAll
  deriving (Functor, Foldable, Generic, Traversable)
  deriving (GEq, GEnum) via Default (Over a)

instance Applicative Over where
  pure x = Over [x]
  OverAll <*> _ = OverAll
  _ <*> OverAll = OverAll
  Over ff <*> Over xx = Over (ff <*> xx)

instance Read a => Read (Over a) where
  readPrec = readP_to_Prec (\prec -> Over  <$> P.sepBy1 (readPrec_to_P readPrec prec) (P.char '+'))

instance Show a => Show (Over a) where
  show (Over xx) = intercalate "+" (map show xx)
  show OverAll = ""

over :: GEnum a => Over a -> [a]
over (Over xx) = xx
over OverAll   = enumerate

---------------------------------------------------------
-- Parsing helpers

char :: Char -> ReadPrec ()
char c = get >>= \c' -> guard (c == c') >> pure ()

string :: String -> ReadPrec ()
string = mapM_ char

dot :: ReadPrec ()
dot = char '.'

---------------------------------------------------------
-- Utils

multi :: [a] -> Bool
multi (_ : _ : _) = True
multi _ = False
