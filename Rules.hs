module Rules where

import Language


alpha :: For -> Bool
alpha x = case x of
        A _ _ -> True
        E _ _ -> True
        _     -> False

beta :: For -> Bool
beta x = case x of
        D _ _ -> True
        I _ _ -> True
        _     -> False

var :: For -> Bool
var x = case x of
    V _ -> True
    _ -> False

insert :: For -> CanSeq -> CanSeq
insert f (Can (xs, ys, zs))
	| var f     = Can (f:xs, ys, zs)
	| beta f    = Can (xs, ys, f:zs)
	| otherwise = Can (xs, f:ys, zs)

{-
ruleAlpha :: CanSeq -> [CanSeq]
ruleAlpha (Can (xs, y:ys, zs)) = case y of
    A n m -> [(insert n (Can (xs,ys, zs))), (insert m (Can (xs, ys, zs)))]
		E n m -> [(insert (N m) (insert n (Can (xs,ys, zs)))), (insert (N n) (insert m (Can (xs, ys, zs))))]
		N (E n m) -> [(insert m (insert n (Can (xs,ys, zs)))), (insert n (insert m (Can (xs, ys, zs))))]
		N (I n m) -> [(insert n (Can (xs,ys, zs))), (insert (N m) (Can (xs, ys, zs)))]
		N (D n m) -> [(insert (N n) (Can (xs,ys, zs))), (insert (N m) (Can (xs, ys, zs)))]

ruleBeta :: CanSeq -> [CanSeq]
ruleBeta (Can (xs, ys, z:zs)) = case z of
        I n m -> [insert m (insert (N n) (Can (xs,ys, zs)))]
		D n m -> [insert m (insert n (Can (xs,ys, zs)))]
		N (A n m) -> [insert (N m) (insert (N n) (Can (xs,ys, zs)))]
-}
{-
ruleBeta :: CanSeq -> [CanSeq]


beta = alternatywa/implikacja/ --nierozgałęzia
alfa = koniunkcja/równoważność --rozgałęzia


minimalne drzewa
mapowanie reguł na sekwnety
-}
