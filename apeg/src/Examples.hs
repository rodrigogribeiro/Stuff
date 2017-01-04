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

import PExp

-- example PEG A -> aA / lambda
{-
aVar :: ScopedSymbol "A" ('("A", String) ': '[]) String
aVar = The (Name :: Name "A")

expVar :: PExp ('("A", String) ': '[]) String
expVar = Var aVar

expr :: PExp ('("A", String) ': '[]) String
expr =  (++) <$> Symb "a" <*> expVar <|> pure []


aPExp :: APExp ('("A", String) ': '[]) ('("A", String) ': '[])
aPExp = Exp expr

aPExp' :: APExp '[] ('("A", String) ': '[])
aPExp' = New (Name :: Name "A") expr

aPeg :: APEG
aPeg = APEG aVar (Next aPExp' Done)

-}
