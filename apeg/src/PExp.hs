{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module PExp where

import Data.Proxy
import Data.Type.Equality
import GHC.TypeLits

-- syntax definition

data Name (s :: Symbol) = KnownSymbol s => Name

data ScopedSymbol (s :: Symbol) (env :: [(Symbol,*)]) (a :: *) =
  (IndexBuilder env s a (AtHead env '(s,a))) => The (Name s)

data PExp (env :: [(Symbol,*)]) (a :: *) where
  Pure :: a -> PExp env a
  Fail :: PExp env a
  Symb :: String -> PExp env String
  Var  :: ScopedSymbol s env a -> PExp env a

-- De Bruijn indices and their generation

data Index (v :: k) (env :: [k]) where
  Here  :: Index v (v ': env)
  There :: Index v env -> Index v (v' ': env)

-- true if s is at list head

type family AtHead (g :: [k]) (s :: k) :: Bool where
  AtHead '[]      s = 'False
  AtHead (t ': g) s = s == t

-- environment  

data Env (env :: [(Symbol,*)]) where
  Nil  :: Env '[]
  (:>) :: KnownSymbol s => (Proxy s, PExp ('(s, a) ': env) a) -> Env env -> Env ('(s, a) ': env)

type family If (b :: Bool) (l :: k) (r :: k) :: k where
  If 'True  l r = l
  If 'False l r = r

type family Lookup (s :: Symbol) (env :: [(Symbol,*)]) :: Maybe * where
  Lookup s '[]             = 'Nothing
  Lookup s ('(t,a) ': env) = If (s == t) ('Just a) (Lookup s env)

class Lookup s env ~ 'Just a =>
      IndexBuilder (env :: [(Symbol,*)])
                   (s :: Symbol)
                   (a :: *)
                   (b :: Bool) where
  index :: Proxy b -> Name s -> Index '(s,a) env 


instance IndexBuilder ('(s, a) ': env) s a 'True where
  index _ _ = Here

instance ((s == t) ~ 'False
         , IndexBuilder g s a
                        (AtHead g '(s, a))) =>
         IndexBuilder ('(t, b) ': g) s a 'False where
  index _ nm = There $ index (Proxy :: Proxy (AtHead g '(s, a))) nm
