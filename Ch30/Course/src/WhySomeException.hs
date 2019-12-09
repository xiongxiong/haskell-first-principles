{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module WhySomeException where

import Control.Exception (ArithException(..), AsyncException(..))
import Data.Typeable

