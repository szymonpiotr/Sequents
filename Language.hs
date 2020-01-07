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
infix 7 `A`
infix 7 `D`
infix 6 `I`



newtype CanSeq = Can ([For], [For], [For]) deriving(Eq,Read,Show)
-- var, alpha, beta

newtype DualSeq = Dual ([For], [For], [For]) deriving(Eq,Read,Show)
-- var, alpha, beta

type GeneralSeq = Either CanSeq DualSeq

type HyperSeq = [GeneralSeq]

data Cal = CanonicalRight | DualRight

{-
ruleAlpha :: CanSeq -> [CanSeq]


ruleBeta :: CanSeq -> [CanSeq]
-}
