module Types where

data Map k a
    = Bin
      {-# UNPACK #-}
      !Size !k a !(Map k a) !(Map k a)
    | Tip

type Size = Int

data Set a 
    = SetBin
      {-# UNPACK #-}
      !Size !a !(Set a) !(Set a)
    | SetTip

-- data Vector a = 
--     Vector {-# UNPACK #-} !Int
--            {-# UNPACK #-} !Int
--            {-# UNPACK #-} !(Array a)
--     deriving (Typeable)