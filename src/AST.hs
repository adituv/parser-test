{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module AST where

import Control.DeepSeq(NFData)
import Data.Int(Int32)
import GHC.Generics(Generic)

data MessageSpec = MessageSpec
  { messageName :: String
  , reservedFields :: [FieldId]
  , knownFields :: [FieldSpec]
  } deriving (Show, Generic, NFData)

data FieldId = TagId Int32 | NameId String
  deriving (Show, Generic, NFData)

data FieldSpec = FieldSpec
  { fieldModifier :: FieldModifier
  , fieldType :: ProtoType
  , fieldName :: String
  , fieldTag :: Int
  } deriving (Show, Generic, NFData)

data ProtoType = TUInt32 | TBool | TString
  deriving (Show, Generic, NFData)

-- Separate type for potential proto2 support as that also has "required" and
-- "optional" modifiers
data FieldModifier = None | Repeated
  deriving (Show, Generic, NFData)