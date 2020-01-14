module Language where

-- | Inductive definition of Formula type
data For =   Verum
            | V Int
            | N For
            | E For For
            | I For For
            | A For For
            | D For For
              deriving(Eq,Read,Show)

infix 9 `V`
infix 8 `N`
infixr 7 `A`
infixr 7 `D`
infixr 6 `I`



newtype CanSeq = Can ([For], [For], [For]) deriving(Eq,Read,Show)
-- var, alpha, beta

newtype DualSeq = Dual ([For], [For], [For]) deriving(Eq,Read,Show)
-- var, alpha, beta

type GeneralSeq = Either CanSeq DualSeq

type HyperSeq = [GeneralSeq]

data Cal = CanonicalRight | DualRight

alpha :: For -> Bool
alpha x = case x of
        A _ _ -> True
	N (D _ _) -> True
	N ( I _ _) -> True
        E _ _ -> True
        _     -> False

beta :: For -> Bool
beta x = case x of
        D _ _ -> True
        I _ _ -> True
	N (A _ _) -> True
	N (E _ _) -> True
        _     -> False

var :: For -> Bool
var x = case x of
    V _ -> True
    N (V _) -> True
    _ -> False

{-
ruleAlpha :: CanSeq -> [CanSeq]


ruleBeta :: CanSeq -> [CanSeq]
-}
