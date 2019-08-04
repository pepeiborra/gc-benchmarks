import Control.Monad
import Data.List
import System.Environment
import System.IO

main = interact parse

parse input =
    case find ("  Gen  1" `isPrefixOf`) (lines input) of
      Just l -> init (last $ words l) ++ "\n"
      Nothing -> ""
