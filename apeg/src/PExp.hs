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

import Control.Applicative
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
  Map  :: (a -> b) -> PExp env a -> PExp env b
  Bind :: PExp env a -> (a -> PExp env b) -> PExp env b
  Symb :: String -> PExp env String
  Var  :: ScopedSymbol s env a -> PExp env a
  Cat  :: PExp env (a -> b) -> PExp env a -> PExp env b
  Alt  :: PExp env a -> PExp env a -> PExp env a
  Star :: PExp env a -> PExp env [a]

instance Functor (PExp env) where
  fmap f = Map f

instance Applicative (PExp env) where
  pure  = Pure
  (<*>) = Cat

instance Monad (PExp env) where
  return = pure
  (>>=)  = Bind
  fail   = const Fail

instance Alternative (PExp env) where
  empty = Fail
  (<|>) = Alt

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

-- type class and its instances for building De Bruijn indices.

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

-- definition of an APExp

data APExp (env :: [(Symbol,*)]) (env' :: [(Symbol,*)]) where
  Exp    :: PExp env a -> APExp env env
  New    :: Lookup s env ~ 'Nothing =>
            Name s ->
            PExp ('(s, a) ': env) a ->
            APExp env ('(s, a) ': env)
  Modify :: ScopedSymbol s env a ->
            PExp env a           ->
            APExp env env
                        
-- productions

data APExps (env :: [(Symbol,*)]) (env' :: [(Symbol,*)]) where
  Done :: APExps env env
  Next :: APExp env env1 -> APExps env1 env' -> APExps env env'

-- adaptable parser expression grammar

data APEG = forall env s a. APEG (ScopedSymbol s env a) (APExps '[] env)

-- examples

