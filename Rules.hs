module Rules where

import Language
import Data.Tree
import Sort
import Text.PrettyPrint

-- insert a singular formula in a sequent, 
-- depending on its form
insert :: For -> CanSeq -> CanSeq
insert f (Can (xs, ys, zs))
	| var f     = Can (f:xs, ys, zs)
	| beta f    = Can (xs, ys, f:zs)
	| otherwise = Can (xs, f:ys, zs)

-- same as above, but for the dual calculus
insertDual :: For -> DualSeq -> DualSeq
insertDual f (Dual (xs, ys, zs))
	| var f	= Dual (f:xs, ys, zs)	 
	| beta f = Dual (xs, f:ys, zs)
	| otherwise = Dual (xs, ys, f:zs)


-- rules for logical connectives  in canonical calculus
-- branching
ruleAlpha :: CanSeq -> [CanSeq]
ruleAlpha (Can (xs, y:ys, zs)) = case y of
    A n m -> [(insert n (Can (xs,ys, zs))), (insert m (Can (xs, ys, zs)))]
    E n m -> [(insert (N m) (insert n (Can (xs,ys, zs)))), (insert (N n) (insert m (Can (xs, ys, zs))))]
    N (E n m) -> [(insert m (insert n (Can (xs,ys, zs)))), (insert n (insert m (Can (xs, ys, zs))))]
    N (I n m) -> [(insert n (Can (xs,ys, zs))), (insert (N m) (Can (xs, ys, zs)))]
    N (D n m) -> [(insert (N n) (Can (xs,ys, zs))), (insert (N m) (Can (xs, ys, zs)))]
	
-- branching rules in dual calculus	
branchingDual :: DualSeq -> [DualSeq]
branchingDual (Dual (xs, y:ys, zs)) = case y of
    D n m -> [(insertDual n (Dual (xs,ys, zs))), (insertDual m (Dual (xs, ys, zs)))]
    N (A n m) -> [(insertDual (N n) (Dual (xs,ys, zs))), (insertDual (N m) (Dual (xs, ys, zs)))]

-- non branching rules in canonical calculus
ruleBeta :: CanSeq -> [CanSeq]
ruleBeta (Can (xs, ys, z:zs)) = case z of
    I n m -> [insert m (insert (N n) (Can (xs,ys, zs)))]
    D n m -> [insert m (insert n (Can (xs,ys, zs)))]
    N (A n m) -> [insert (N m) (insert (N n) (Can (xs,ys, zs)))]

-- checks if a given sequent is atomic
-- that is there are only variables left
atomicseq (Can (x, y, z)) = if ((null y && null z) || checkform y z) then True else False 

-- checks if there are corresponding formula
-- in two given lists of formulas
checkform :: [For] -> [For] -> Bool
checkform [] ys = False
checkform (x:xs) ys = case x of 
	N z -> z `elem` ys
	z -> (N z) `elem` ys 

-- applies a given rule to a singular sequent depending on
-- formulas it's comprised of
applyrule :: CanSeq -> [CanSeq]
applyrule (Can (x, y, z)) 
		| atomicseq (Can (x, y, z))  = [Can (x, y, z)]
		| not (null z) 		   = ruleBeta (Can (x, y, z)) 
		| otherwise 		   = ruleAlpha (Can (x, y, z)) 

-- using the above function to work on hypersequent
applyrule1 :: [CanSeq] -> [CanSeq]
applyrule1 (x:xs) = if atomicseq x then (x:applyrule1 xs) else (applyrule x) ++ xs
applyrule1 [] = []  

-- builds a tree using rule applying rules to hypersequent
buildtree :: CanSeq -> Tree [CanSeq]
buildtree x = Node [x] [Node (applyrule x) []]

-- similar as above but the argument is of a tree type
prooftree :: Tree [CanSeq] -> Tree [CanSeq]
prooftree (Node x []) 
    	| all atomicseq x = (Node x [])
        | not (all atomicseq x) = (Node x [(Node (applyrule1 x) [])])
prooftree (Node x xs) = Node x (map prooftree xs) 

-- checks if tree is atomic, that is all of the leaves are atomic
atom_tree (Node x []) = if all atomicseq x then True else False 
atom_tree (Node x [z]) = atom_tree z 

-- builds a derivation for a hypersequent
derivation :: [CanSeq] -> Tree [CanSeq]
derivation x = until (atom_tree) prooftree (Node x [])

-- similar as above, but starting with a singular formula
derivationF :: For -> Tree [CanSeq]
derivationF x = derivation [sortCan x]

-- returns complementary formulas
compl :: For -> For
compl x =  case x of 
	N z -> z
	z -> N z

-- checks if a given formula is an element of a sequent
elemseq :: For -> DualSeq -> Bool
elemseq x (Dual (xs, ys, zs)) = elem x xs || elem x ys || elem x zs

-- returns a lidt of complementary formulas
returncompl :: [For] -> [For] -> [For]
returncompl [] zs = []
returncompl xs [] = []
returncompl (x:xs) zs = if elem (compl x) zs then x:(compl x):returncompl xs zs else returncompl xs zs


-- given two sequents, function returns list of complementary formulas
compllist :: DualSeq -> DualSeq -> [For]
compllist (Dual (x, y, z)) (Dual (x1, y1, z1)) = returncompl x x1 ++ returncompl y z1 ++ returncompl z y1

test1 = Dual ([(V 1), (V 2)], [], [])
test2 = Dual ([N (V 1), N (V 2), N (V 1)], [], [])

test3 = Dual ([], [], [N (D (V 1) (V 2))])
test4 = Dual ([], [D (V 1) (V 2)], [])

test5 = Dual ([V 1], [], [])
test6 = Dual ([V 2], [], [])
test7 = Dual ([N (V 1)], [], [])



-- resolution
fun :: [For] -> DualSeq -> DualSeq -> DualSeq
fun (x1:x2:xs) (Dual (z1, z2, z3)) (Dual (y1, y2, y3)) = case var x1 of 
	True -> Dual (filter (/= x1) z1 ++ filter (/= x2) y1, z2 ++ y2, z3 ++ y3)  
	False -> case beta x1 of
		True -> Dual (y1 ++ z1, filter (/= x1) z2 ++ y2, filter (/= x2) y3 ++ z3)
		False -> Dual (y1 ++ z1, filter (/= x2) y2 ++ z2, filter (/= x1) z3 ++ y3)


-- finding a sequent in a list by means of which we can employ resolution method
fun2 :: DualSeq -> [DualSeq] -> [DualSeq]
fun2 x [] = [x]
fun2 x (y:ys) = if c == [] then fun2 x ys ++ [y] else
		(fun c x y):ys ++ [x, y]	
		where 
			c = compllist x y
	  

-- checking if dual hypersequent is closed -- all lists are empty
closedhyp :: [DualSeq] -> Bool
closedhyp (x:xs) = any (\x -> x == Dual ([], [], [])) xs

-- building proof tree using resolution only
prooftree2 :: Tree [DualSeq] -> Tree [DualSeq]
prooftree2 (Node (x:xs) []) = (Node (x:xs) [(Node (fun2 x xs) [])])
prooftree2 (Node x xs) = Node x (map prooftree2 xs)

-- as above, but starting with a hypersequent
derdual :: [DualSeq] -> Tree [DualSeq]
derdual (x:xs) = until (fun3) prooftree2 (Node (x:xs) [])

-- resolution on first hyperseqeunt fulfilling criteria for resolution
fun3 :: Tree [DualSeq] -> Bool
fun3 (Node (x:xs) []) = fun2 x xs == (x:xs)
fun3 (Node (x:xs) [z]) = fun3 z


-- printing formulas
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
