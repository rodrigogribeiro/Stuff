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

import Data.Functor.Identity
import Control.Applicative hiding (many)
import Data.Proxy
import Data.Type.Equality
import GHC.TypeLits

import Text.Parsec hiding ((<|>), Empty)

import Unsafe.Coerce

data PExp (env :: [(Symbol,*)]) (a :: *) where
  Pure :: a -> PExp env a
  Fail :: PExp env a
  Map  :: (a -> b) -> PExp env a -> PExp env b
  Bind :: PExp env a -> (a -> PExp env b) -> PExp env b
  Sym  :: String -> PExp env String
  Var  :: ( KnownSymbol s
          , Lookup s env ~ 'Just a) => Proxy s -> PExp env a
  Cat  :: PExp env (a -> b) -> PExp env a -> PExp env b
  Alt  :: PExp env a -> PExp env a -> PExp env a
  Star :: PExp env a -> PExp env [a]
  Not  :: PExp env a -> PExp env ()

instance Functor (PExp  env) where
  fmap f = Map f

instance Applicative (PExp env) where
  pure  = Pure
  (<*>) = Cat

instance Alternative (PExp env) where
  empty = Fail
  (<|>) = Alt

instance Monad (PExp env) where
  return = pure
  (>>=)  = Bind
  
data Env (xs :: [(Symbol,*)]) where
  Nil :: Env '[]
  (:*) :: ( KnownSymbol s
          , Lookup s xs ~ 'Nothing )
          => (Proxy s, PExp ('(s,a) ': xs) a) -> Env xs -> Env ('(s,a) ': xs)

infixr 5 :*

type family If (b :: Bool) (l :: k) (r :: k) :: k where
    If 'True  l r = l
    If 'False l r = r

type family Lookup (s :: Symbol) (env :: [(Symbol,*)]) :: Maybe * where
    Lookup s '[]             = 'Nothing
    Lookup s ('(t,a) ': env) = If (s == t) ('Just a) (Lookup s env)

type family DropWhileNotSame (s :: Symbol) (xs :: [(Symbol,*)]) :: [(Symbol,*)] where
  DropWhileNotSame s '[] = '[]
  DropWhileNotSame s ('(x,a) ': xs) = If (s == x) ('(x,a) ': xs) (DropWhileNotSame s xs)


dropWhileNotSame :: (KnownSymbol s
                    , Lookup s env ~ 'Just a)
                    => Proxy s
                    -> Env env
                    -> Env (DropWhileNotSame s env)
dropWhileNotSame s ((s', p) :* rho)
  = case sameOrNotSymbol s s' of
       Left Refl -> dropWhileNotSame s rho
       Right Refl -> (s', p) :* rho

sameOrNotSymbol :: (KnownSymbol a, KnownSymbol b)
                => Proxy a -> Proxy b -> Either ((a == b) :~: 'False) (a :~: b)
sameOrNotSymbol s s' = maybe (Left $ unsafeCoerce Refl) Right $ sameSymbol s s'

look :: ( Lookup s xs ~ 'Just a
        , KnownSymbol s)
        => Proxy s -> Env xs -> PExp (DropWhileNotSame s xs) a
look s ((s',p) :* xs)
  = case sameOrNotSymbol s s' of
       Left Refl -> look s xs
       Right Refl -> p


update :: ( Lookup s env ~ 'Just a
          , KnownSymbol s )
          => Proxy s
          -> PExp (DropWhileNotSame s env) a
          -> Env env
          -> Env env
update s p ((s', p') :* rho)
  = case sameOrNotSymbol s s' of
       Left Refl  -> (s',p') :* update s p rho
       Right Refl -> (s, p' <|> p) :* rho

insert :: ( Lookup s env ~ 'Nothing
          , KnownSymbol s )
          => Proxy s
          -> PExp ('(s,a) ': env) a
          -> Env env
          -> Env ('(s,a) ': env)
insert s p rho
  = (s , p) :* rho


type Parser a = ParsecT String () Identity a

interp :: Env env -> PExp env a -> Parser a
interp rho (Pure v)
  = pure v
interp rho Fail
  = fail "parse error"
interp rho (Map f p)
  = f <$> (interp rho p)
interp rho (Bind p f)
  = interp rho p >>= interp rho . f
interp rho (Sym s)
  = string s
interp rho (Var v)
  = interp (dropWhileNotSame v rho) (look v rho)
interp rho (Cat p p')
  = interp rho p <*> interp rho p'
interp rho (Alt p p')
  = interp rho p <|> interp rho p'
interp rho (Star p)
  = many (interp rho p)


data APEG (env :: [(Symbol,*)])
  = APEG {
      prods :: Env env            
    }

modifyRule :: ( Lookup s env ~ 'Just a
              , KnownSymbol s )
              => Proxy s
              -> PExp (DropWhileNotSame s env) a
              -> APEG env
              -> APEG env
modifyRule s p (APEG pr)
  = APEG (update s p pr)


insertRule :: ( Lookup s env ~ 'Nothing
              , KnownSymbol s )
              => Proxy s
              -> PExp ('(s,a) ': env) a
              -> APEG env
              -> APEG ('(s,a) ': env)
insertRule s p (APEG pr)
  = APEG (insert s p pr)

emptyAPEG :: APEG '[]
emptyAPEG = APEG Nil
