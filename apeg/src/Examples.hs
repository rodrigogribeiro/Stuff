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

module Examples where

import Control.Applicative
import Data.Proxy
import PExp

test :: Env '[ '("a",Bool), '("b",Int), '("c",Bool)]
test = (Proxy :: Proxy "a", undefined) :*
       (Proxy :: Proxy "b", undefined) :*
       (Proxy :: Proxy "c", undefined) :* Nil


aVar :: PExp '[ '("A",String) ] String
aVar = Var (Proxy :: Proxy "A")

expr :: PExp '[ '("A", String) ] String
expr = (++) <$> Sym "a" <*> aVar <|> pure []

apeg :: APEG '[ '("A", String) ]
apeg = insertRule (Proxy :: Proxy "A") expr emptyAPEG

