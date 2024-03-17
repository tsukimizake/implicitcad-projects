module Lib where

with :: ([a] -> b) -> a -> a -> b
with f a b = f [a, b]
