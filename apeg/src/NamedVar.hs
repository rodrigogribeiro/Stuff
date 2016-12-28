{-# OPTIONS -Wall                  #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE StandaloneDeriving    #-}

module NamedVar where

import GHC.TypeLits
import Data.Proxy
import Data.Type.Equality

---------------------------------------------------
-- Well-typed and well-scoped De Bruijn indices  --
---------------------------------------------------

data Index (g :: [k]) (s :: k) where
  Zero :: Index (s ': g) s
  Succ :: Index g s -> Index (t ': g) s
deriving instance Show (Index g s)


toInt :: Index g s -> Int
toInt Zero       = 0
toInt (Succ idx) = 1 + toInt idx

indexEq :: Index g s -> Index g t -> Maybe (s :~: t)
indexEq v w = case (v, w) of
  (Zero   , Zero)    -> Just Refl
  (Succ v', Succ w') -> indexEq v' w'
  _                  -> Nothing
 
-- type level if operator

type family If (b :: Bool) (l :: k) (r :: k) :: k where
  If 'True  l r = l
  If 'False l r = r

-- membership in an environment

type family HasSymbol (g :: [(Symbol,*)]) (s :: Symbol) :: Maybe * where
  HasSymbol '[]            s = 'Nothing
  HasSymbol ('(t, a) ': g) s = If (s == t) ('Just a) (HasSymbol g s)

-- named variables definition

data Name (s :: Symbol) = KnownSymbol s => Var

instance Show (Name a) where
  show Var = symbolVal (Proxy :: Proxy a)

type family AtHead (g :: [k]) (s :: k) :: Bool where
  AtHead '[]      s = 'False
  AtHead (t ': g) s = s == t  

data ScopedSymbol (s :: Symbol) (g :: [(Symbol,*)]) (a :: *) =
  (HasSymbolIdx g s a (AtHead g '(s, a))) => The (Name s)

instance Show (ScopedSymbol s g a) where
  show v@(The nm) = concat [ show nm, " (= " , show (index'' v),  ")" ]  

-- reification of variables

class HasSymbol g s ~ 'Just a =>
      HasSymbolIdx (g :: [(Symbol,*)]) (s :: Symbol) (a :: *) (b :: Bool) where
  index :: Proxy b -> Name s -> Index g '(s, a)

instance HasSymbolIdx ('(s, a) ': g) s a 'True where
  index _ _ = Zero

instance ((s == t) ~ 'False, HasSymbolIdx g s a (AtHead g '(s, a))) =>
         HasSymbolIdx ('(t, b) ': g) s a 'False where
  index _ nm = Succ $ index' nm

index' :: forall g s a. HasSymbolIdx g s a (AtHead g '(s, a)) => Name s -> Index g '(s, a)
index' nm = index (Proxy :: Proxy (AtHead g '(s, a))) nm

index'' :: forall g s a. ScopedSymbol s g a -> Index g '(s, a)
index'' (The nm) = index (Proxy :: Proxy (AtHead g '(s, a))) nm
