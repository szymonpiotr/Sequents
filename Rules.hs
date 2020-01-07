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
	
	
atomicseq :: CanSeq -> Bool
atomicseq (Can (x, y, z)) = if (null y && null z) then True else False

applyrule :: CanSeq -> [CanSeq]
applyrule (Can (x, y, z)) 
		| atomicseq (Can (x, y, z))  = [Can (x, y, z)]
		| not (null z) 		   = ruleBeta (Can (x, y, z)) 
		| otherwise 		   = ruleAlpha (Can (x, y, z)) 

applyrule1 :: [CanSeq] -> [CanSeq]
applyrule1 (x:xs) = if atomicseq x then (x:applyrule1 xs) else (applyrule x) ++ xs
applyrule1 [] = []  


buildtree :: CanSeq -> RoseTree [CanSeq]
buildtree x = Node [x] [Node (applyrule x) []]

prooftree :: RoseTree [CanSeq] -> RoseTree [CanSeq]
prooftree (Node x []) 
	| all atomicseq x = (Node x [])
        | not (all atomicseq x) = (Node x [(Node (applyrule1 x) [])])
prooftree (Node x xs) = Node x (map prooftree xs) 


atom_tree (Node x []) = if all atomicseq x then True else False 
atom_tree (Node x [z]) = atom_tree z 

derivation :: [CanSeq] -> RoseTree [CanSeq]
derivation x = until (atom_tree) prooftree (Node x [])




f1 = I (A (I (N (V 1)) (D (V 2) (V 1))) (D (V 1) (V 2))) (N (V 2)) 
-- p v ~p
f2 = D (V 1) (N (V 1))
-- (p v ¬q) → (r & q)
f3 = I (D (V 1) (N (V 3))) (A (V 10) (V 3))
-- absrd
f4 = I (I (V 1) (V 2)) (I (I (V 1) (N (V 2))) (N (V 1) ))

f5 = I (D (N (V 1)) (V 2)) (V 3)


{-
ruleBeta :: CanSeq -> [CanSeq]


beta = alternatywa/implikacja/ --nierozgałęzia
alfa = koniunkcja/równoważność --rozgałęzia


minimalne drzewa
mapowanie reguł na sekwnety
-}
