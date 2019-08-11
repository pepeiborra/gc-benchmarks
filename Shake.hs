{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications #-}

import Development.Shake hiding (Normal)
import Data.Maybe
import System.Directory
import System.FilePath
import Text.Read

import Analysis

-- Could be dynamic
sizes :: [Int]
sizes = [25, 50, 100, 200, 400, 800, 1600]

main :: IO ()
main = shakeArgs shakeOptions $ do
  phony "install" $ do
    readmeLines <- readFileLines "README.md"
    let links = map (drop 2 . dropWhile (/= ':'))$ takeWhile (not.null) $ reverse $ readmeLines
        traces = mapMaybe (readMaybe @Trace) links
        analyses = mapMaybe (readMaybe @Analysis) links
    Just out <- getEnv "out"
    liftIO $ createDirectoryIfMissing True out
    mapM_ ((\x -> copyFile' x (out </> x)))
      $  (show <$> enumerate @DataSet)
      ++ (show <$> traces)
      ++ (show <$> analyses)

  rule @RunLog $ \RunLog {..} out -> do
    need [takeDirectory out </> show program]
    Stderr res <- cmd
      ("./" <> show program)
      (  [show (size * 1000), argMode mode, "+RTS", "-S"]
      ++ [ "-xn" | Incremental <- [gc] ]
      )
    writeFile' out res

  rule @DataSet $ \DataSet {..} out -> do
    values <- mapM
      (fmap (parserFor metric) . readFile' . (takeDirectory out </>) . show)
      [ RunLog { .. } :: RunLog | size <- sizes ]
    writeFile' out
               (unlines $ zipWith (\a b -> unwords [show a, b]) sizes values)

  rule @Trace $ \t out -> do
    plotTrace t out

  rule @Analysis $ \analysis out -> do
    plotAnalysis analysis out

  "PusherBS" %> \out -> do
    need ["Pusher.hs"]
    cmd "ghc" ["-threaded", "-rtsopts", "-O2", "Pusher.hs", "-o", out]

  "PusherDouble" %> \out -> do
    need ["PusherDouble.hs"]
    cmd "ghc" ["-threaded", "-rtsopts", "-O2", "PusherDouble.hs", "-o", out]

-- | A helper for defining rules over 'Read'able typed file paths
rule :: forall a . Read a => (a -> String -> Action ()) -> Rules ()
rule k = (isJust . readMaybe @a . takeFileName) ?> \out -> k (read @a (takeFileName out)) out

-- Local Variables:
-- dante-methods: (bare-ghci)
-- End:
