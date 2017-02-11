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
{-# LANGUAGE StandaloneDeriving #-}

module PExp where

import Data.Functor.Identity
import Control.Applicative hiding (many)
import Data.Proxy
import Data.Type.Equality
import GHC.TypeLits

import Prelude hiding (lookup)
 
data Subset (env :: [(Symbol,*)]) (env' :: [(Symbol,*)]) where
  Done :: Subset '[] '[]
  Skip :: forall s a env env'. Subset env env' -> Subset env ('(s,a) ': env')
  Keep :: forall s a a' env env'. Subset env env' -> Subset ('(s,a) ': env) ('(s,a') ': env')

deriving instance Show (Subset env env')

type family SameHead (env :: [(Symbol,*)]) (env' :: [(Symbol,*)]) :: Bool where
  SameHead '[] '[] = 'True
  SameHead xs '[]  = 'False
  SameHead '[] ys  = 'False
  SameHead ('(s,a) ': xs) ('(s',b) ': ys) = s == s'

class IsSubset (env :: [(Symbol,*)]) (env' :: [(Symbol,*)]) (c :: Bool) where
  subset :: Proxy c -> Subset env env'

instance IsSubset '[] '[] 'True where
  subset _ = Done

instance ( (s == s') ~ 'False
         , IsSubset ('(s,a) ': env) env' (SameHead ('(s,a) ': env) env'))
         => IsSubset ('(s,a) ': env) ('(s',a') ': env') 'False where
  subset _ = Skip (subset (Proxy :: Proxy (SameHead ('(s,a) ': env) env')))

instance IsSubset env env' (SameHead env env') => IsSubset ('(s,a') ': env) ('(s,a) ': env') 'True where
  subset _ = Keep (subset (Proxy :: Proxy (SameHead env env')))

