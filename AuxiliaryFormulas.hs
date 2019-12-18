module AuxiliaryFormulas
--  (
--    subFormulas
--  , sortComplexity
--  , atoms
--  , noRepAtoms
--  , permuteAtoms
--  )
where

import Language
import Data.List

-- | For a given forumula generate a list of its subformulas (+ negations).
subFormulas :: For -> [For]
subFormulas x = case x of
    Verum -> []
    V x   -> [V x,N (V x)]
    N x   -> nub $ N x:N (N x):subFormulas x
    E x y -> nub $ E x y:N (E x y):subFormulas x ++ subFormulas y
    I x y -> nub $ I x y:N (I x y):subFormulas x ++ subFormulas y
    A x y -> nub $ A x y:N (A x y):subFormulas x ++ subFormulas y
    D x y -> nub $ D x y:N (D x y):subFormulas x ++ subFormulas y

-- | Quicksort a list of formulas with respect to their complexity.
sortComplexity :: [For] -> [For]
sortComplexity x = case x of
  []     -> []
  (x:xs) -> case (isAtomic x) of
      True  -> sortComplexity left ++ sortComplexity right
      False -> sortComplexity left ++ [x] ++ sortComplexity right
      where
        left  = [z | z <- xs,formulaComplexity z <= formulaComplexity x]
        right = [z' | z' <- xs,formulaComplexity z' > formulaComplexity x]
        formulaComplexity :: For -> Int
        formulaComplexity x = case x of
          Verum -> 0
          V _   -> 0
          N x   -> 1 + formulaComplexity x
          E y z -> 1 + formulaComplexity y + formulaComplexity z
          I y z -> 1 + formulaComplexity y + formulaComplexity z
          A y z -> 1 + formulaComplexity y + formulaComplexity z
          D y z -> 1 + formulaComplexity y + formulaComplexity z
        isAtomic :: For -> Bool
        isAtomic for = case for of
          Verum   -> True
          V _     -> True
          N (V _) -> True
          _       -> False

-- | For a given formula generate all atoms occuring in it.
atoms :: For -> [For]
atoms for = case for of
            Verum -> []
            V x   -> [V x]
            N y   -> atoms y
            E x y -> atoms x ++ atoms y
            I x y -> atoms x ++ atoms y
            A x y -> atoms x ++ atoms y
            D x y -> atoms x ++ atoms y

-- | Remove copies of atoms.
noRepAtoms :: For -> [For]
noRepAtoms = nub.atoms

-- | Generate list of all permutations of atoms.
permuteAtoms :: [For] -> [[For]]
permuteAtoms atoms = permutations atoms

-- | The size (number of branches) of a maximal tree as a function of imput formulas
size :: For -> Int
size f = 2^len
  where
    len = length $ noRepAtoms f

-- | levels of a tree
levelsCutFor :: [For] -> [[For]]
levelsCutFor xs = (head per) : [(replicate 2 (head (head (tail per))))]
  where
    per = permutations xs

exportIndex :: For -> Int
exportIndex (V n) = n

exportIndex1 :: [For] -> [Int]
exportIndex1 xs = map exportIndex xs

exportIndex11 :: [[For]] -> [[Int]]
exportIndex11 xs = map exportIndex1 xs
