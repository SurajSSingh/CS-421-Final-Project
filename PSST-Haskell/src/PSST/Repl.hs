-- Repl code adapted from https://blogg.bekk.no/creating-a-repl-in-haskell-efcdef1deec2
module PSST.Repl
    (repl
    ) where

import PSST.Core(Exp (..), Val (..), Diagnostic (..))
import PSST.Parser
import PSST.Evaler
import System.IO
import Control.Monad

readString :: IO String
readString = putStr "PSST-REPL >>> "
       >> hFlush stdout
       >> getLine

repl :: IO () -> IO ()
repl main = do
    input <- readString

    if input == "exit" || input == "quit"
        then return ()
        else
         case psstEval (psstParser input) of
            Right val -> print val
            Left err -> print err
        >> main