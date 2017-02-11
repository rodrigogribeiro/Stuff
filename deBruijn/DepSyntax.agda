open import Data.List
open import Data.Nat
open import Data.Vec

module DepSyntax where

  data In {A : Set}(x : A) : List A -> Set where
    Here  : forall {xs} -> In x (x ∷ xs)
    There : forall {y ys} -> In x ys -> In x (y ∷ ys)
