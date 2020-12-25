module Algorithms.Decision where

{-
    Represents a decision made by an Algorithm.
    Every algorithm does not calculate the same thing.
    We want to know what the algorithms result actually recommends us to do.
-}
data Decision = Buy
              | Keep
              | Sell
              deriving (Show, Eq)
