{-# LANGUAGE TemplateHaskell            #-}
module Language.Haskell.LSP.Types.TextDocument where

import           Data.Text                      ( Text )
import           Language.Haskell.LSP.Types.Constants
import           Language.Haskell.LSP.Types.Location
import           Language.Haskell.LSP.Types.Uri

-- ---------------------------------------------------------------------
{-
TextDocumentIdentifier

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#textdocumentidentifier

Text documents are identified using a URI. On the protocol level, URIs are
passed as strings. The corresponding JSON structure looks like this:

interface TextDocumentIdentifier {
    /**
     * The text document's URI.
     */
    uri: string;
}
-}
data TextDocumentIdentifier =
  TextDocumentIdentifier
    { _textDocumentIdentifierUri :: Uri
    } deriving (Show, Read, Eq)

deriveLspJSON lspOptions ''TextDocumentIdentifier

{-
TextDocumentItem

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#textdocumentitem

    New: An item to transfer a text document from the client to the server.

interface TextDocumentItem {
    /**
     * The text document's URI.
     */
    uri: string;

    /**
     * The text document's language identifier.
     */
    languageId: string;

    /**
     * The version number of this document (it will strictly increase after each
     * change, including undo/redo).
     */
    version: number;

    /**
     * The content of the opened text document.
     */
    text: string;
}
-}

data TextDocumentItem =
  TextDocumentItem {
    _textDocumentItemUri :: Uri
  , _textDocumentItemLanguageId :: Text
  , _textDocumentItemVersion    :: Int
  , _textDocumentItemText       :: Text
  } deriving (Show, Read, Eq)

deriveLspJSON lspOptions ''TextDocumentItem

-- ---------------------------------------------------------------------
{-
TextDocumentPositionParams

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#textdocumentpositionparams

    Changed: Was TextDocumentPosition in 1.0 with inlined parameters


interface TextDocumentPositionParams {
    /**
     * The text document.
     */
    textDocument: TextDocumentIdentifier;

    /**
     * The position inside the text document.
     */
    position: Position;
}

-}
data TextDocumentPositionParams =
  TextDocumentPositionParams
    { _textDocumentPositionParamsTextDocument :: TextDocumentIdentifier
    , _textDocumentPositionParamsPosition     :: Position
    } deriving (Show, Read, Eq)

deriveLspJSON lspOptions ''TextDocumentPositionParams
