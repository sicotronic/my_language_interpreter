module Iswim.Main where

import Iswim.Iswim
import Iswim.Domains
import Iswim.Eval
import Iswim.Translate
import Imp.Syntax

run :: Dom -> IO ()
run (Lnorm _ _) = return ()
run (Iswim.Domains.Error err _) = putStrLn err
run TypeError = putStrLn "Type error" 
run (Lout n d) = putStrLn (show n) >> run d
run (Lin f) = getLine >>= \line -> 
              case reads line of
                [(x,"")] -> run (f x)
                _ -> putStrLn "Error in the input"


main :: String -> IO ()
main input = run (eval (translate (parse_command input)) ninit sinit)

-- Example of use:
-- main "catch l in fail l; newvar y:=23 {! y} with newvar x:=0 { while x<7 do x:=x+1; ! x od; if not (false => true) then newvar y:=0 { for y:=0 to 6 do ! y od } else skip fi }"
