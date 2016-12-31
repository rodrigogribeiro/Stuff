{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module PExp where

import Data.Proxy
import Data.Type.Equality
import GHC.TypeLits

data Exp (env :: [(Symbol,*)]) (a :: *) where
  Val :: Int -> Exp env Int

data Index (v :: k) (env :: [k]) where
  Here  :: Index v (v ': env)
  There :: Index v env -> Index v (v' ': env)

data Env (env :: [(Symbol,*)]) where
  Nil  :: Env '[]
  (:>) :: KnownSymbol s => (Proxy s, a) -> Env env -> Env ('(s, a) ': env)

type family If (b :: Bool) (l :: k) (r :: k) :: k where
  If 'True  l r = l
  If 'False l r = r

type family (s :: Symbol) `Elem` (env :: [(Symbol,*)]) :: Maybe * where
  s `Elem` '[] = 'Nothing
  s `Elem` ('(t,a) ': env) = If (s == t) ('Just a) (s `Elem` env)

search :: KnownSymbol s => Index '(s,a) env -> Env env -> a
search Here (x :> _) = snd x
search (There ix) (_ :> env) = search ix env
