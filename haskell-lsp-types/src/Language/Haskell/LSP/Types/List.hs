-- Need to split these types out into a separate module since
-- ClientCapabilities also depends on them
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DeriveGeneric              #-}
module Language.Haskell.LSP.Types.List where

import           Control.DeepSeq
import qualified Data.Aeson                                 as A
import           Data.Aeson.Types
import           GHC.Generics

-- | This data type is used to host a FromJSON instance for the encoding used by
-- elisp, where an empty list shows up as "null"
newtype List a = List [a]
                deriving (Show,Read,Eq,Ord,Monoid,Functor,Foldable,Traversable,Generic)

instance NFData a => NFData (List a)

instance (A.ToJSON a) => A.ToJSON (List a) where
  toJSON (List ls) = toJSON ls

instance (A.FromJSON a) => A.FromJSON (List a) where
  parseJSON A.Null = return (List [])
  parseJSON v      = List <$> parseJSON v

instance Semigroup (List a) where
  (<>) = mappend
