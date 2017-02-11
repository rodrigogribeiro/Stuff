{-# LANGUAGE GADTs, TypeFamilies, DataKinds, KindSignatures, RankNTypes, TypeOperators #-}

module Syntax where

data Nat = Zero | Succ Nat
           deriving (Eq, Ord, Show)
               
data SNat (n :: Nat) where
    SZero :: SNat 'Zero
    SSucc :: SNat n -> SNat ('Succ n)
    
data Exp (n :: Nat) (a :: *) where
    Var :: SNat n -> Exp n a
    Val :: Int -> Exp n Int
    
data Env (n :: Nat) (xs :: [*]) where
    Nil  :: Env 'Zero '[] 
    (:>) :: forall n a xs. Exp n a -> Env n xs -> Env ('Succ n) (a ': xs)
        
type family (n :: Nat) :<= (m :: Nat) :: Bool where
    'Zero :<= m = 'True
    ('Succ n) :<= 'Zero = 'False
    ('Succ n) :<= ('Succ m) = n :<= m   
    
type family TypeAt (n :: Nat)(xs :: [*]) :: Maybe * where
    TypeAt n '[] = 'Nothing
    TypeAt 'Zero (x ': xs) = 'Just x
    TypeAt ('Succ n) (x ': xs) = TypeAt n xs
    
lookupEnv :: ( n :<= m ~ 'True
             , TypeAt n xs ~ 'Just a)
             => SNat n 
             -> Env ('Succ m) xs 
             -> Exp ('Succ n) a
lookupEnv SZero (e :> _) = e
lookupEnv (SSucc ix) (_ :> rho) = undefined --lookupEnv ix rho             