{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.LSP.Types.Constants where

import           Data.Char (toLower)
import           Language.Haskell.TH
import           Data.Aeson.TH
import qualified Data.ByteString.Char8 as BC

-- ---------------------------------------------------------------------

-- | Standard options for use when generating JSON instances
lspOptions :: Options
lspOptions = defaultOptions { omitNothingFields = True, fieldLabelModifier = dropDefaultPrefix }
 -- NOTE: This needs to be in a separate file because of the TH stage restriction

dropDefaultPrefix :: String -> String
dropDefaultPrefix = drop 1

dropLabelPrefix :: String -> String -> String
dropLabelPrefix name label =
  let base = BC.unpack $ last $ BC.split '.' $ BC.pack name
      pascal = drop (length base) $ dropDefaultPrefix label in

      if null pascal
        then error $ "Invalid field name for JSON entity. The field label for '"++label++"' must be prefixed by '"++base++"'"
        else '_':toLower (head pascal):tail pascal

deriveLspJSON :: Options -> Name -> Q [Dec]
deriveLspJSON options name =
  let baseLabel = dropLabelPrefix $ show name
      defaultLabel = fieldLabelModifier options in
      deriveJSON (options { fieldLabelModifier = defaultLabel . baseLabel }) name
