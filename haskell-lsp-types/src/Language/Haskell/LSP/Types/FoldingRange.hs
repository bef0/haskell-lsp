{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Language.Haskell.LSP.Types.FoldingRange where

import qualified Data.Aeson                    as A
import           Data.Text                    (Text)
import           Language.Haskell.LSP.Types.Constants
import           Language.Haskell.LSP.Types.List
import           Language.Haskell.LSP.Types.TextDocument
import           Language.Haskell.LSP.Types.Message

data FoldingRangeParams =
  FoldingRangeParams
  { _foldingRangeParamsTextDocument :: TextDocumentIdentifier -- ^ The text document.
  }
  deriving (Read, Show, Eq)

deriveLspJSON lspOptions ''FoldingRangeParams

-- | Enum of known range kinds
data FoldingRangeKind = FoldingRangeComment
                        -- ^ Folding range for a comment
                      | FoldingRangeImports
                        -- ^ Folding range for a imports or includes
                      | FoldingRangeRegion
                        -- ^ Folding range for a region (e.g. #region)
                      | FoldingRangeUnknown Text
                        -- ^ Folding range that haskell-lsp-types does
                        -- not yet support
  deriving (Read, Show, Eq)

instance A.ToJSON FoldingRangeKind where
  toJSON FoldingRangeComment     = A.String "comment"
  toJSON FoldingRangeImports     = A.String "imports"
  toJSON FoldingRangeRegion      = A.String "region"
  toJSON (FoldingRangeUnknown x) = A.String x

instance A.FromJSON FoldingRangeKind where
  parseJSON (A.String "comment") = pure FoldingRangeComment
  parseJSON (A.String "imports") = pure FoldingRangeImports
  parseJSON (A.String "region")  = pure FoldingRangeRegion
  parseJSON (A.String x)         = pure (FoldingRangeUnknown x)
  parseJSON _                    = mempty

-- | Represents a folding range.
data FoldingRange =
  FoldingRange
  { -- | The zero-based line number from where the folded range starts.
    _foldingRangeStartLine      :: Int
    -- | The zero-based character offset from where the folded range
    -- starts. If not defined, defaults to the length of the start line.
  , _foldingRangeStartCharacter :: Maybe Int
    -- | The zero-based line number where the folded range ends.
  , _foldingRangeEndLine        :: Int
    -- | The zero-based character offset before the folded range ends.
    -- If not defined, defaults to the length of the end line.
  , _foldingRangeEndCharacter   :: Maybe Int
    -- | Describes the kind of the folding range such as 'comment' or
    -- 'region'. The kind is used to categorize folding ranges and used
    -- by commands like 'Fold all comments'. See 'FoldingRangeKind' for
    -- an enumeration of standardized kinds.
  , _foldingRangeKind           :: Maybe FoldingRangeKind
  }
  deriving (Read, Show, Eq)

deriveLspJSON lspOptions ''FoldingRange

type FoldingRangeRequest = RequestMessage ClientMethod FoldingRangeParams (List FoldingRange)
type FoldingRangeResponse = ResponseMessage (List FoldingRange)
