import Development.Shake
import Data.List
import System.Directory
import System.FilePath

-- Could be dynamic
sizes :: [Int]
sizes = [25, 50, 100, 200, 400, 800, 1600, 3200]

label "runtimes" = "Runtime (s)"
label "pauses" = "Max Gen1 pause (s)"
label other  = error $ "no label for: " ++ other

main :: IO ()
main = shakeArgs shakeOptions $ do
  want ["runtimes.svg", "pauses.svg"]

  -- 5000.xn.log
  -- 5000.noxn.log
  "*.log" %> \out -> do
    need ["Pusher"]
    let useXn = ".xn" == takeExtension (dropExtension out)
        arg   = dropExtension $ takeBaseName out
    Stderr res <- cmd
      "./Pusher"
      ([show (read arg * 1000), "+RTS", "-s"] ++ [ "-xn" | useXn ])
    writeFile' out res

  -- pauses.noxn.dataset
  -- pauses.xn.dataset
  "*.dataset" %> \out -> do
    let name   = dropExtension $ takeBaseName out
        mode   = takeExtension $ dropExtension out -- xn / noxn
        parser = case name of
          "pauses"   -> parsePause
          "runtimes" -> parseRuntime
          other      -> error $ "No parser for " ++ other

    values <- mapM (fmap parser . readFile')
                   [ show n ++ mode ++ ".log" | n <- sizes ]
    writeFile' out
               (unlines $ zipWith (\a b -> unwords [show a, b]) sizes values)

  "*.svg" %> \out -> do
    let name = dropExtension $ takeFileName out
    need [ name <.> mode <.> "dataset" | mode <- ["noxn", "xn"] ]
    Stdout graph <- cmd
      "graph"
      [ "-Tsvg"
      , "-S"
      , "3"
      , "-C"
      , name <.> "noxn" <.> "dataset"
      , "-S"
      , "5"
      , "-C"
      , name <.> "xn" <.> "dataset"
      , "-X"
      , "Map size (K)"
      , "-Y"
      , label name
      ]
    writeFile' out graph

  "Pusher" %> \out ->
    cmd "ghc" ["-threaded", "-rtsopts", "-O2", "Pusher.hs"]

  phony "install" $ do
    Just out <- getEnv "out"
    liftIO $ createDirectoryIfMissing True out
    mapM_
      (\x -> copyFile' x (out </> x))
      [ "runtimes.svg"
      , "runtimes.xn.dataset"
      , "runtimes.noxn.dataset"
      , "pauses.svg"
      , "pauses.xn.dataset"
      , "pauses.noxn.dataset"
      , "Pusher"
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
