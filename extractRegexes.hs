import Skylighting.Syntax
import qualified Data.Map as M
import Skylighting.Parser

main = do
  let contexts = M.elems $ M.map sContexts syntaxMap
  let res = filter isRE $ map rMatcher $ concatMap (concat . M.elems . M.map cRules) contexts
  mapM_ printRE res

printRE :: Matcher -> IO ()
printRE (RegExpr r) = putStrLn $ reString r
printRE _ = return ()

isRE :: Matcher -> Bool
isRE RegExpr{} = True
isRE _ = False
