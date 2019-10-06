{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

import qualified Data.ByteString   as BS
import           Data.Maybe
import           Development.Shake hiding (Normal)
import           System.Directory
import           System.FilePath
import           Text.Read

import           Analysis

-- Could be dynamic
sizes :: MsgTypeSing -> [Int]
sizes S = takeWhile (<= 1200) (sizes B)
sizes _ = [25, 50, 100, 200, 400, 800, 1200, 1400, 1800]

-- Looks for a paragraph of markdown references at the end of a list of lines
extractLinksFromLines :: [String] -> [String]
extractLinksFromLines =
    map (drop 2 . dropWhile (/= ':'))
    . takeWhile (not . null)
    . reverse

is :: forall a . Read a => String -> Bool
is = isJust . readMaybe @a

main :: IO ()
main = shakeArgs shakeOptions $ do
  phony "install" $ do
    Just out <- getEnv "out"
    liftIO $ createDirectoryIfMissing True out
    readmeLines <- readFileLines "README.md"
    let links = filter (\x -> is @Trace x || is @Analysis x) $ extractLinksFromLines readmeLines
    mapM_ ((\x -> copyFile' x (out </> x)))
      $ links ++ ["README.html"]

  rule @RunLog $ \RunLog {..} out -> do
    need [takeDirectory out </> "Pusher"]
    Stderr res <- cmd
      (WithStderr False)
      "./Pusher"
      (  [show (size * 1000), argMode mode, show msgType]
      ++ concat [ [show ms, show n] | IncrementalWithPauses _ ms n <- [gc]]
      ++ ["+RTS", "-S"]
      ++ concat [ ["-xn", "-N" <> show c] | Incremental c <- [gc] ]
      ++ concat [ ["-xn", "-N" <> show c] | IncrementalWithPauses c _ _ <- [gc] ]
      )
    writeFile' out res

  rule @DataSet $ \DataSet {..} out -> do
    values <- mapM
      (fmap (parserFor metric) . readFile' . (takeDirectory out </>) . show)
      [ RunLog { .. } :: RunLog | size <- sizes msgType ]
    writeFile' out
               (unlines $ zipWith (\a b -> unwords [show a, b]) (sizes msgType) values)

  rule @Trace $ \t out -> do
    putNormal $ "Plotting trace: " <> show t
    plotTrace t out

  rule @Analysis $ \analysis out -> do
    putNormal $ "Plotting analysis: " <> show analysis
    plotAnalysis analysis out

  "Pusher" %> \out -> do
    need [out <.> "hs"]
    cmd "ghc" ["-main-is", (takeFileName out), "-threaded", "-rtsopts", "-O2", out <.> "hs", "-o", out]

  "*.html" %> \out -> do
    let md = replaceExtension out "md"
    readmeLines <- readFileLines md
    let links = filter (\x -> is @Trace x || is @Analysis x) $ extractLinksFromLines readmeLines
    need links
    Stdout html <- cmd "pandoc" [md]
    liftIO $ BS.writeFile out html

  phony "clean" $ do
    cmd "rm" "*.hi *.o *.dataset *.log"

-- | A helper for defining rules over 'Read'able typed file paths
rule :: forall a . Read a => (a -> String -> Action ()) -> Rules ()
rule k = (isJust . readMaybe @a . takeFileName) ?> \out -> k (read @a (takeFileName out)) out

argMode :: Mode -> [Char]
argMode Normal = "5"
argMode ExtraIterations = "10"

-- Local Variables:
-- dante-methods: (bare-ghci)
-- End:
