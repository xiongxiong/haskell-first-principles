{-# LANGUAGE FlexibleInstances #-}

module Ex1 where

import Test.QuickCheck

data Sum b a = First a | Second b

instance Functor (Sum e) where
    fmap f (First a) = First (f a)
    fmap _ (Second b) = Second b

data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c

data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'

data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)

data K a b = K a

instance Functor (K a) where
    fmap _ (K a) = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
    fmap f (Flip (K a)) = Flip (K (f a))

data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst a) = GoatyConst (f a)

data LiftItOut f a = LiftItOut (f a)

instance Functor (LiftItOut Maybe) where
    fmap _ (LiftItOut Nothing) = LiftItOut Nothing
    fmap f (LiftItOut (Just x)) = LiftItOut (Just (f x))

data Parappa f g a = DaWrappa (f a) (g a)

data Para1 a = Para1 a

data Para2 a = Para2 a

instance Functor (Parappa Para2 Para1) where
    fmap f (DaWrappa (Para2 a) (Para1 a')) = DaWrappa (Para2 (f a)) (Para1 (f a'))

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor (IgnoreOne Para1 Para2 Int) where
    fmap f (IgnoringSomething (Para1 a) (Para2 b)) = IgnoringSomething (Para1 a) (Para2 (f b))

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor (Notorious Para1 Int Float) where
    fmap f (Notorious (Para1 a) (Para1 b) (Para1 c)) = Notorious (Para1 a) (Para1 b) (Para1 (f c))

data List a = Nil | Cons a (List a)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)

data GoatLord a = 
        NoGoat
    |   OneGoat a
    |   MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print a b) = Print a (f b)
    fmap f (Read e) = Read (fmap f e)
    -- fmap f (Read e) = Read (f . e)
