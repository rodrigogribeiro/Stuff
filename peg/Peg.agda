

module Peg where

  -- dependency tree

  data Tree : Set where
    leaf     : Tree
    straight : Tree → Tree
    branch   : Tree → Tree → Tree
