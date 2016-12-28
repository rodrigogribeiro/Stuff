module Lib where

-- free monad definition

data Free f r = Free (f (Free f r)) | Pure r

-- defining free as a monad

instance Functor f => Functor (Free f r) where
  fmap f (Pure r)  = Pure r
  fmap f (Free fs) = Free (fmap f <$> fs)
