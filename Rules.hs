module Rules where

import Language
import Data.Tree
import Sort
import Text.PrettyPrint

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
	
	
atomicseq (Can (x, y, z)) = if ((null y && null z) || checkform y z) then True else False 

checkform :: [For] -> [For] -> Bool
checkform [] ys = False
checkform (x:xs) ys = case x of 
	N z -> z `elem` ys
	z -> (N z) `elem` ys 


applyrule :: CanSeq -> [CanSeq]
applyrule (Can (x, y, z)) 
		| atomicseq (Can (x, y, z))  = [Can (x, y, z)]
		| not (null z) 		   = ruleBeta (Can (x, y, z)) 
		| otherwise 		   = ruleAlpha (Can (x, y, z)) 

applyrule1 :: [CanSeq] -> [CanSeq]
applyrule1 (x:xs) = if atomicseq x then (x:applyrule1 xs) else (applyrule x) ++ xs
applyrule1 [] = []  


buildtree :: CanSeq -> Tree [CanSeq]
buildtree x = Node [x] [Node (applyrule x) []]

prooftree :: Tree [CanSeq] -> Tree [CanSeq]
prooftree (Node x []) 
    	| all atomicseq x = (Node x [])
        | not (all atomicseq x) = (Node x [(Node (applyrule1 x) [])])
prooftree (Node x xs) = Node x (map prooftree xs) 


atom_tree (Node x []) = if all atomicseq x then True else False 
atom_tree (Node x [z]) = atom_tree z 

derivation :: [CanSeq] -> Tree [CanSeq]
derivation x = until (atom_tree) prooftree (Node x [])

derivationF :: For -> Tree [CanSeq]
derivationF x = derivation [sortCan x]


-- zwraca formułę komplementarna
compl :: For -> For
compl x =  case x of 
	N z -> z
	z -> N z

-- czy dana formuła jest elementem sekwentu
elemseq :: For -> DualSeq -> Bool
elemseq x (Dual (xs, ys, zs)) = elem x xs || elem x ys || elem x zs

returncompl :: [For] -> [For] -> [For]
returncompl [] zs = []
returncompl xs [] = []
returncompl (x:xs) zs = if elem (compl x) zs then x:(compl x):returncompl xs zs else returncompl xs zs


-- bierze dwa sekwenty i wypluwa liste formuł komplementarnych
compllist :: DualSeq -> DualSeq -> [For]
compllist (Dual (x, y, z)) (Dual (x1, y1, z1)) = returncompl x x1 ++ returncompl y z1 ++ returncompl z y1

test1 = Dual ([(V 1), (V 2)], [], [])
test2 = Dual ([N (V 1), N (V 2), N (V 1)], [], [])

test3 = Dual ([], [], [N (D (V 1) (V 2))])
test4 = Dual ([], [D (V 1) (V 2)], [])

test5 = Dual ([V 1], [], [])
test6 = Dual ([V 2], [], [])
test7 = Dual ([N (V 1)], [], [])



-- rezolucja
fun :: [For] -> DualSeq -> DualSeq -> DualSeq
fun (x1:x2:xs) (Dual (z1, z2, z3)) (Dual (y1, y2, y3)) = case var x1 of 
	True -> Dual (filter (/= x1) z1 ++ filter (/= x2) y1, z2 ++ y2, z3 ++ y3)  
	False -> case beta x1 of
		True -> Dual (y1 ++ z1, filter (/= x1) z2 ++ y2, filter (/= x2) y3 ++ z3)
		False -> Dual (y1 ++ z1, filter (/= x2) y2 ++ z2, filter (/= x1) z3 ++ y3)


-- znalezienie sekwentu w liscie, z uwagi na ktorego mozna zastosowac rezolucje wzgledem pierwszego
-- 
fun2 :: DualSeq -> [DualSeq] -> [DualSeq]
fun2 x [] = [x]
fun2 x (y:ys) = if c == [] then fun2 x ys ++ [y] else
		(fun c x y):ys ++ [x, y]	
		where 
			c = compllist x y
	  

printFor :: For -> String
printFor for = case for of
    (V y)    -> "p" ++ (show y)
    (N y)    -> "~" ++ "(" ++ (printFor (y)) ++ ")"
    (E y z)  -> "(" ++ printFor (y) ++ ")" ++ " = " ++ "(" ++ printFor (z) ++ ")"
    (I y z)  -> "(" ++ printFor (y) ++ ")" ++ " -> " ++ "(" ++ printFor (z) ++ ")"
    (A y z)  -> "(" ++ printFor (y) ++ ")" ++ " & " ++ "(" ++ printFor (z) ++ ")"
    (D y z)  -> "(" ++ printFor (y) ++ ")" ++ " v " ++ "(" ++ printFor (z) ++ ")"

mapPrint :: [For] -> String
mapPrint (x:xs) = "[" ++ printFor x ++ ", "  ++ mapPrint xs
mapPrint [] = "]"

printSeq :: CanSeq -> String
printSeq  (Can (x, y, z)) = "Can " ++ "([" ++ mapPrint x  ++ "]" ++ ",[" ++ mapPrint y  ++ "]" ++ ",[" ++ mapPrint z  ++ "])" 

mapPrint2 :: [CanSeq] -> String
mapPrint2 (x:xs) = "[" ++ printSeq x ++ ", "  ++ mapPrint2 xs
mapPrint2 [] = "]"


canseq2string :: Tree [CanSeq] -> Tree String
canseq2string (Node x []) = Node (mapPrint2 x) []
canseq2string (Node x xs) = Node (mapPrint2 x) (map canseq2string xs) 


f1 = I (A (I (N (V 1)) (D (V 2) (V 1))) (D (V 1) (V 2))) (N (V 2)) 
-- p v ~p
f2 = D (V 1) (N (V 1))
-- (p v ¬q) → (r & q)
f3 = I (D (V 1) (N (V 3))) (A (V 10) (V 3))
-- absrd
f4 = I (I (V 1) (V 2)) (I (I (V 1) (N (V 2))) (N (V 1) ))

f5 = I (D (N (V 1)) (V 2)) (V 3)

f6 = (A (V 1) (V 2)) `D` N (A (V 1) (V 2))



{-
ruleBeta :: CanSeq -> [CanSeq]


beta = alternatywa/implikacja/ --nierozgałęzia
alfa = koniunkcja/równoważność --rozgałęzia


minimalne drzewa
mapowanie reguł na sekwnety
-}
