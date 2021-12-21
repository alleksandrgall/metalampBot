module Test where

{-| List double 
>>> doublelist "a"
"aa"
prop> \(l::[Int]) -> length (doublelist l) == 2 * length l
NOW +++ OK, passed 100 tests.
-}
doublelist :: [a] -> [a]
doublelist l = l ++ l 
