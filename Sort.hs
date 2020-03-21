module Sort where

import Language

-- sorts elements of a given list to one of three
-- depending on its category (variable, alpha, beta)
sortList :: [For] -> ([For],[For],[For]) -> ([For],[For],[For])
sortList [] (a,b,c) = (a,b,c)
sortList (x:xs) (a,b,c)
    | var x   = sortList xs (x:a,b,c)
    | alpha x = sortList xs (a,x:b,c)
    | beta x  = sortList xs (a,b,x:c)

-- similar as the function above, but with singular formula
sortFormula :: For -> ([For],[For],[For])
sortFormula x = sortList [x] ([],[],[])

-- creates Canonical sequent using sortFormula
sortCan :: For -> CanSeq
sortCan x = Can $ sortFormula x

-- changes alphas with betas
dualize :: (a,a,a) -> (a,a,a)
dualize (x,y,z) = (x,z,y)

-- creates Dual sequent using sortFormula and dualize
sortDual :: For -> DualSeq
sortDual x = Dual $ dualize (sortFormula x)

-- sort funtion working for both Canonical and Dual calculus
sort :: Cal -> [For] -> GeneralSeq
sort cal xs = case cal of
  CanonicalRight -> Left (Can $ sortList xs ([],[],[]))
  DualRight      -> Right (Dual $ sortList xs ([],[],[]))
