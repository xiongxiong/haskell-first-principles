{-# LANGUAGE RankNTypes #-}

module Nat where

type Nat f g = forall a . f a -> g a