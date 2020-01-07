module Sort where

import Language
import Rules

sortList :: [For] -> ([For],[For],[For]) -> ([For],[For],[For])
sortList [] (a,b,c) = (a,b,c)
sortList (x:xs) (a,b,c)
    | var x   = sortList xs (x:a,b,c)
    | alpha x = sortList xs (a,x:b,c)
    | beta x  = sortList xs (a,b,x:c)

sortFormula :: For -> ([For],[For],[For])
sortFormula x = sortList [x] ([],[],[])

sortCan :: For -> CanSeq
sortCan x = Can $ sortFormula x

dualize :: (a,a,a) -> (a,a,a)
dualize (x,y,z) = (x,z,y)

sortDual :: For -> DualSeq
sortDual x = Dual $ dualize (sortFormula x)

sort :: Cal -> [For] -> GeneralSeq
sort cal xs = case cal of
  CanonicalRight -> Left (Can $ sortList xs ([],[],[]))
  DualRight      -> Right (Dual $ sortList xs ([],[],[]))
