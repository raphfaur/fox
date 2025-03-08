
import Backend (State, Variables (Dict), evalExpr, newExpr, runState, parseAll, empty)
import Frontend ()
import Debug.Trace (trace)
import System.Directory.Internal.Prelude (getArgs)

-- evalAll :: Maybe([Expr], String) -> State Variables Int
-- evalAll (Just([], _)) = return 0
-- evalAll (Just(e:exprs, _)) = evalExpr e >>= newExpr (evalAll (Just (exprs, "")))

-- test:: Maybe(Expr, String) -> IO ()
-- test (Just([], _)) = return ()
-- test (Just(e:exprs, _)) = do
--     case e of 
--         (Print e) -> print $ show v
--         _ -> return ()
--     where Just (v,d) = runState (evalExpr e) (Dict empty)

run :: String -> IO()
run file = do
  s <- readFile file
  let res = runState (evalExpr $ parseAll s) (Dict empty empty) in print res

parse :: String -> IO()
parse file = do
  s <- readFile file
  print $ parseAll s

main = do 
  args <- getArgs
  case args of 
    [file] -> run file