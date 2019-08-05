import Development.Shake
import qualified Data.ByteString as BS
import Data.Char
import Data.List
import System.Directory
import System.FilePath

-- Could be dynamic
sizes :: [Int]
sizes = [25, 50, 100, 200, 400, 800, 1600, 3200]

label "runtimes" = "Runtime (s)"
label "pauses" = "Max Gen1 pause (s)"
label "maxResidency" = "Max Residency (bytes)"
label other  = error $ "no label for: " ++ other

program ".bs" = "Pusher"
program ".double" = "PusherDouble"
program other = error $ "no program for: " ++ other

main :: IO ()
main = shakeArgs shakeOptions $ do
  want
    ["runtimes.bs.svg", "pauses.bs.svg", "maxResidency.bs.svg",
     "runtimes.double.svg", "pauses.double.svg", "maxResidency.double.svg"]

  -- 5000.double.xn.log
  -- 5000.bs.noxn.log
  "*.log" %> \out -> do
    let useXn = ".xn" == takeExtension (dropExtension out)
        arg   = dropExtensions $ takeBaseName out
        ext   = takeExtension $ dropExtension $ dropExtension out
    need [program ext]
    Stderr res <- cmd
      ("./" <> program ext)
      ([show (read arg * 1000), "+RTS", "-s"] ++ [ "-xn" | useXn ])
    writeFile' out res

  -- pauses.double.noxn.dataset
  -- pauses.bs.xn.dataset
  "*.dataset" %> \out -> do
    let name   = dropExtensions $ takeBaseName out
        ext    = takeExtension  $ dropExtension out
        mode   = takeExtensions $ dropExtension out -- bs.xn / double.noxn
        parser = case name of
          "pauses"   -> parsePause
          "runtimes" -> parseRuntime
          "maxResidency" -> parseMaxResidency
          other      -> error $ "No parser for " ++ other

    values <- mapM (fmap parser . readFile')
                   [ show n ++ mode ++ ".log" | n <- sizes ]
    writeFile' out
               (unlines $ zipWith (\a b -> unwords [show a, b]) sizes values)

  -- pauses.double.svg
  -- runtimes.bs.svg
  "*.svg" %> \out -> do
    let name = dropExtensions $ takeFileName out
        ext  = takeExtension  $ dropExtension out
    need [ name <.> ext <.> mode <.> "dataset" | mode <- ["noxn", "xn"] ]
    Stdout bs <- cmd
      "graph"
      [ "-Tsvg"
      , "-S"
      , "3"
      , "-C"
      , name <.> ext <.> "noxn" <.> "dataset"
      , "-S"
      , "5"
      , "-C"
      , name <.> ext <.> "xn" <.> "dataset"
      , "-X"
      , "Map size (K)"
      , "-Y"
      , label name
      ]
    liftIO $ BS.writeFile out bs

  "Pusher" %> \out -> do
    need ["Pusher.hs"]
    cmd "ghc" ["-threaded", "-rtsopts", "-O2", "Pusher.hs"]

  "PusherDouble" %> \out -> do
    need ["PusherDouble.hs"]
    cmd "ghc" ["-threaded", "-rtsopts", "-O2", "PusherDouble.hs"]

  phony "install" $ do
    Just out <- getEnv "out"
    liftIO $ createDirectoryIfMissing True out
    mapM_ (\x -> copyFile' x (out </> x)) $ concat
      [ [ "runtimes" <.> ext <.> "svg"
      , "runtimes" <.> ext <.> "xn.dataset"
      , "runtimes" <.> ext <.> "noxn.dataset"
      , "pauses" <.> ext <.> "svg"
      , "pauses" <.> ext <.> "xn.dataset"
      , "pauses" <.> ext <.> "noxn.dataset"
      , "maxResidency" <.> ext <.> "svg"
      , "maxResidency" <.> ext <.> "xn.dataset"
      , "maxResidency" <.> ext <.> "noxn.dataset"
      ]
      | ext <- ["bs", "double"]
      ]

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