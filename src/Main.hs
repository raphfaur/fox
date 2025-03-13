import Backend (Env (Dict), empty, evalExpr, parseAll, runState)
import Frontend ()
import System.Directory.Internal.Prelude (getArgs)
import Codegen (compile)

run :: String -> IO ()
run file = do
  s <- readFile file
  let res = runState (evalExpr $ parseAll s) ([Dict empty empty], Dict empty empty) in case res of
    Left e -> print e
    Right d -> print d

parse :: String -> IO ()
parse file = do
  s <- readFile file
  print $ parseAll s

main :: IO ()
main = do
  args <- getArgs
  case args of
    (cmd : file : _)  -> case cmd of 
      "parse" -> parse file
      "run" -> run file
      "compile" -> compile file
