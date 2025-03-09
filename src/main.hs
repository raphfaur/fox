import Backend (State, Env (Dict), empty, evalExpr, newExpr, parseAll, runState)
import Debug.Trace (trace)
import Frontend ()
import System.Directory.Internal.Prelude (getArgs)

run :: String -> IO ()
run file = do
  s <- readFile file
  let res = runState (evalExpr $ parseAll s) ([Dict empty empty], Dict empty empty) in print res

parse :: String -> IO ()
parse file = do
  s <- readFile file
  print $ parseAll s

main = do
  args <- getArgs
  case args of
    [file] -> run file