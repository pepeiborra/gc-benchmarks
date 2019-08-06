{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Applicative
import Control.Monad
import Development.Shake hiding (Normal)
import qualified Data.ByteString as BS
import Data.Char
import Data.List
import Data.List.Split
import Generics.Deriving
import GHC.Generics
import System.Directory
import System.FilePath
import Text.Read
import Text.ParserCombinators.ReadPrec

-- Could be dynamic
sizes :: [Int]
sizes = [25, 50, 100, 200, 400, 800, 1600, 3200]

data Metric = Runtimes | Pauses | MaxResidency
  deriving (Bounded, Generic, Read, Show)
  deriving (GEq, GEnum) via Default Metric

label Runtimes = "Runtime (s)"
label Pauses = "Max Gen1 pause (s)"
label MaxResidency = "Max Residency (bytes)"

parserFor Pauses = parsePause
parserFor Runtimes = parseRuntime
parserFor MaxResidency = parseMaxResidency

data Mode = Normal | ExtraIterations
  deriving (Generic, Bounded, Read, Show)
  deriving (GEq, GEnum) via Default Mode

argMode Normal = "5"
argMode ExtraIterations = "10"

data Program = PusherBS | PusherDouble
  deriving (Generic, Bounded, Read, Show)
  deriving (GEq, GEnum) via Default Program

data GC = Copying | Incremental
  deriving (Generic, Bounded, Read, Show)
  deriving (GEq, GEnum) via Default GC

data RunLog = RunLog
  { gc   :: GC
  , mode :: Mode
  , program :: Program
  , size :: Int
  }

instance Show RunLog where show RunLog{..} = show size <.> show mode <.> show program <.> show gc <.> ext @RunLog
instance Read RunLog where
  readPrec = do
    size <- readPrec <* dot
    mode <- readPrec <* dot
    program <- readPrec <* dot
    gc <- readPrec <* dot
    _ext <- many get
    guard $ _ext == ext @RunLog
    return RunLog{..}

data DataSet = DataSet
  { metric :: Metric
  , gc      :: GC
  , mode    :: Mode
  , program :: Program
  }
  deriving (Generic)
  deriving (GEq, GEnum) via Default DataSet

instance Show DataSet where show DataSet{..} = show metric <.> show program <.> show mode <.> show gc <.> ext @DataSet
instance Read DataSet where
  readPrec = do
    metric <- readPrec <* dot
    program <- readPrec <* dot
    mode <- readPrec <* dot
    gc <- readPrec <* dot
    _ext <- many get
    guard $ _ext == ext @DataSet
    return DataSet{..}

data Analysis
  = OverGC { metric :: Metric , mode :: Mode , program :: Program}
  | OverMode { metric :: Metric, gc :: GC, program :: Program }
  deriving (Generic)
  deriving (GEq, GEnum) via Default Analysis

title OverGC{..}   = show metric <> " by GC - " <> show program
title OverMode{..} = show metric <> " by # iterations - " <> show gc <> " - " <> show program

toDataSets :: Analysis -> [DataSet]
toDataSets OverGC{..}   = [DataSet{..} | gc <- enumerate]
toDataSets OverMode{..} = [DataSet{..} | mode <- enumerate]

analysisMetric OverGC{metric}   = metric
analysisMetric OverMode{metric} = metric

instance Show Analysis where
  show OverGC{..}   = show metric <.> show program <.> show mode <.> ext @Analysis
  show OverMode{..} = show metric <.> show program <.> show gc   <.> ext @Analysis
instance Read Analysis where
  readPrec = readOverGC <|> readOverMode
    where
      readOverMode = do
        metric <- readPrec <* dot
        program <- readPrec <* dot
        gc <- readPrec <* dot
        _ext <- many get
        guard $ _ext == ext @Analysis
        return OverMode{..}
      readOverGC = do
        metric <- readPrec <* dot
        program <- readPrec <* dot
        mode <- readPrec <* dot
        _ext <- many get
        guard $ _ext == ext @Analysis
        return OverGC{..}

class HasExt a where ext :: String
instance HasExt RunLog where ext = "log"
instance HasExt DataSet where ext = "dataset"
instance HasExt Analysis where ext = "svg"

rule :: forall a . HasExt a => String
rule = "//*." <> ext @a

main :: IO ()
main = shakeArgs shakeOptions $ do
  phony "install" $ do
    Just out <- getEnv "out"
    liftIO $ createDirectoryIfMissing True out
    mapM_ ((\x -> copyFile' x (out </> x))) $
      (show <$> enumerate @DataSet) ++
      (show <$> enumerate @Analysis)

  rule @RunLog %> \out -> do
    let RunLog{..} = read (takeFileName out)
    need [takeDirectory out </> show program]
    Stderr res <- cmd ("./" <> show program)
                      ([show (size * 1000), argMode mode, "+RTS", "-s"] ++ [ "-xn" | Incremental <- [gc]])
    writeFile' out res

  rule @DataSet %> \out -> do
    let DataSet{..} = read (takeFileName out)
    values <- mapM (fmap (parserFor metric) . readFile' . (takeDirectory out </>) . show) [ RunLog{..} | size <- sizes ]
    writeFile' out (unlines $ zipWith (\a b -> unwords [show a, b]) sizes values)

  -- pauses.double.svg
  -- runtimes.bs.svg
  rule @Analysis %> \out -> do
    let analysis = read (takeFileName out)
        datasets = toDataSets analysis
    need (map ((takeDirectory out </>) . show) datasets)
    Stdout bs <- cmd
      "graph"
      [ "-Tsvg"
      , "-L"
      , title analysis
      , "-title-font-size"
      , "0.05"
      , "-S"
      , "3"
      , "-C"
      , show (datasets!!0)
      , "-S"
      , "5"
      , "-C"
      , show (datasets!!1)
      , "-X"
      , "Map size (K)"
      , "-Y"
      , show (analysisMetric analysis)
      ]
    liftIO $ BS.writeFile out bs

  "PusherBS" %> \out -> do
    need ["Pusher.hs"]
    cmd "ghc" ["-threaded", "-rtsopts", "-O2", "Pusher.hs", "-o", out]

  "PusherDouble" %> \out -> do
    need ["PusherDouble.hs"]
    cmd "ghc" ["-threaded", "-rtsopts", "-O2", "PusherDouble.hs", "-o", out]

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
