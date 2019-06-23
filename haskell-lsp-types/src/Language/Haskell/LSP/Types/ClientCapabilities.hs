{-# LANGUAGE TemplateHaskell        #-}

module Language.Haskell.LSP.Types.ClientCapabilities where

import qualified Data.Aeson as A
import Data.Default
import Language.Haskell.LSP.Types.Constants
import Language.Haskell.LSP.Types.CodeAction
import Language.Haskell.LSP.Types.Completion
import Language.Haskell.LSP.Types.List
import Language.Haskell.LSP.Types.MarkupContent
import Language.Haskell.LSP.Types.Symbol

-- ---------------------------------------------------------------------
{-
New in 3.0
----------

/**
 * Workspace specific client capabilities.
 */
export interface WorkspaceClientCapabilities {
        /**
         * The client supports applying batch edits to the workspace by supporting
         * the request 'workspace/applyEdit'
         */
        applyEdit?: boolean;

        /**
         * Capabilities specific to `WorkspaceEdit`s
         */
        workspaceEdit?: {
                /**
                 * The client supports versioned document changes in `WorkspaceEdit`s
                 */
                documentChanges?: boolean;
        };

        /**
         * Capabilities specific to the `workspace/didChangeConfiguration` notification.
         */
        didChangeConfiguration?: {
                /**
                 * Did change configuration notification supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `workspace/didChangeWatchedFiles` notification.
         */
        didChangeWatchedFiles?: {
                /**
                 * Did change watched files notification supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `workspace/symbol` request.
         */
        symbol?: {
                /**
                 * Symbol request supports dynamic registration.
                 */
                dynamicRegistration?: boolean;

                /**
                 * Specific capabilities for the `SymbolKind` in the `workspace/symbol` request.
                 */
                symbolKind?: {
                        /**
                         * The symbol kind values the client supports. When this
                         * property exists the client also guarantees that it will
                         * handle values outside its set gracefully and falls back
                         * to a default value when unknown.
                         *
                         * If this property is not present the client only supports
                         * the symbol kinds from `File` to `Array` as defined in
                         * the initial version of the protocol.
                         */
                        valueSet?: SymbolKind[];
                }
        };

        /**
         * Capabilities specific to the `workspace/executeCommand` request.
         */
        executeCommand?: {
                /**
                 * Execute command supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * The client has support for workspace folders.
         *
         * Since 3.6.0
         */
        workspaceFolders?: boolean;

        /**
         * The client supports `workspace/configuration` requests.
         *
         * Since 3.6.0
         */
        configuration?: boolean;
}
-}

-- -------------------------------------

data WorkspaceEditClientCapabilities =
  WorkspaceEditClientCapabilities
  { _workspaceEditClientCapabilitiesDocumentChanges :: Maybe Bool -- ^The client supports versioned document
                                   -- changes in `WorkspaceEdit`s
  } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''WorkspaceEditClientCapabilities)

-- -------------------------------------

data DidChangeConfigurationClientCapabilities =
  DidChangeConfigurationClientCapabilities
    { _didChangeConfigurationClientCapabilitiesynamicRegistration :: Maybe Bool -- ^Did change configuration
                                         -- notification supports dynamic
                                         -- registration.
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''DidChangeConfigurationClientCapabilities)

-- -------------------------------------

data DidChangeWatchedFilesClientCapabilities =
  DidChangeWatchedFilesClientCapabilities
    { _didChangeWatchedFilesClientCapabilitiesDynamicRegistration :: Maybe Bool -- ^Did change watched files
                                         -- notification supports dynamic
                                         -- registration.
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''DidChangeWatchedFilesClientCapabilities)

-- -------------------------------------

data SymbolKindClientCapabilities =
  SymbolKindClientCapabilities
   { -- | The symbol kind values the client supports. When this
     -- property exists the client also guarantees that it will
     -- handle values outside its set gracefully and falls back
     -- to a default value when unknown.
     --
     -- If this property is not present the client only supports
     -- the symbol kinds from `File` to `Array` as defined in
     -- the initial version of the protocol.
     _symbolKindClientCapabilitiesValueSet :: Maybe (List SymbolKind)
   } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''SymbolKindClientCapabilities)

instance Default SymbolKindClientCapabilities where
  def = SymbolKindClientCapabilities (Just $ List allKinds)
    where allKinds = [ SkFile
                     , SkModule
                     , SkNamespace
                     , SkPackage
                     , SkClass
                     , SkMethod
                     , SkProperty
                     , SkField
                     , SkConstructor
                     , SkEnum
                     , SkInterface
                     , SkFunction
                     , SkVariable
                     , SkConstant
                     , SkString
                     , SkNumber
                     , SkBoolean
                     , SkArray
                     ]

data SymbolClientCapabilities =
  SymbolClientCapabilities
    { _symbolClientCapabilitiesDynamicRegistration :: Maybe Bool -- ^Symbol request supports dynamic
                                         -- registration.
    , _symbolClientCapabilitiesSymbolKind :: Maybe SymbolKindClientCapabilities -- ^ Specific capabilities for the `SymbolKind`.
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''SymbolClientCapabilities)

-- -------------------------------------

data ExecuteClientCapabilities =
  ExecuteClientCapabilities
    { _executeClientCapabilitiesDynamicRegistration :: Maybe Bool -- ^Execute command supports dynamic
                                         -- registration.
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''ExecuteClientCapabilities)

-- -------------------------------------

data WorkspaceClientCapabilities =
  WorkspaceClientCapabilities
    { -- | The client supports applying batch edits to the workspace by supporting
      -- the request 'workspace/applyEdit'
      _workspaceClientCapabilitiesApplyEdit :: Maybe Bool

      -- | Capabilities specific to `WorkspaceEdit`s
    , _workspaceClientCapabilitiesWorkspaceEdit :: Maybe WorkspaceEditClientCapabilities

      -- | Capabilities specific to the `workspace/didChangeConfiguration` notification.
    , _workspaceClientCapabilitiesDidChangeConfiguration :: Maybe DidChangeConfigurationClientCapabilities

       -- | Capabilities specific to the `workspace/didChangeWatchedFiles` notification.
    , _workspaceClientCapabilitiesDidChangeWatchedFiles :: Maybe DidChangeWatchedFilesClientCapabilities

      -- | Capabilities specific to the `workspace/symbol` request.
    , _workspaceClientCapabilitiesSymbol :: Maybe SymbolClientCapabilities

      -- | Capabilities specific to the `workspace/executeCommand` request.
    , _workspaceClientCapabilitiesExecuteCommand :: Maybe ExecuteClientCapabilities

      -- | The client has support for workspace folders.
    , _workspaceClientCapabilitiesWorkspaceFolders :: Maybe Bool

      -- | The client supports `workspace/configuration` requests.
    , _workspaceClientCapabilitiesConfiguration :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''WorkspaceClientCapabilities)

instance Default WorkspaceClientCapabilities where
  def = WorkspaceClientCapabilities def def def def def def def def

-- ---------------------------------------------------------------------
{-
New in 3.0
----------
/**
 * Text document specific client capabilities.
 */
export interface TextDocumentClientCapabilities {

        synchronization?: {
                /**
                 * Whether text document synchronization supports dynamic registration.
                 */
                dynamicRegistration?: boolean;

                /**
                 * The client supports sending will save notifications.
                 */
                willSave?: boolean;

                /**
                 * The client supports sending a will save request and
                 * waits for a response providing text edits which will
                 * be applied to the document before it is saved.
                 */
                willSaveWaitUntil?: boolean;

                /**
                 * The client supports did save notifications.
                 */
                didSave?: boolean;
        }

        /**
         * Capabilities specific to the `textDocument/completion`
         */
        completion?: {
                /**
                 * Whether completion supports dynamic registration.
                 */
                dynamicRegistration?: boolean;

                /**
                 * The client supports the following `CompletionItem` specific
                 * capabilities.
                 */
                completionItem?: {
                        /**
                         * Client supports snippets as insert text.
                         *
                         * A snippet can define tab stops and placeholders with `$1`, `$2`
                         * and `${3:foo}`. `$0` defines the final tab stop, it defaults to
                         * the end of the snippet. Placeholders with equal identifiers are linked,
                         * that is typing in one will update others too.
                         */
                        snippetSupport?: boolean;

                        /**
                         * Client supports commit characters on a completion item.
                         */
                        commitCharactersSupport?: boolean

                        /**
                         * Client supports the follow content formats for the documentation
                         * property. The order describes the preferred format of the client.
                         */
                        documentationFormat?: MarkupKind[];

                        /**
                         * Client supports the deprecated property on a completion item.
                         */
                        deprecatedSupport?: boolean;

                        /**
                         * Client supports the preselect property on a completion item.
                         */
                        preselectSupport?: boolean;
                }

                completionItemKind?: {
                        /**
                         * The completion item kind values the client supports. When this
                         * property exists the client also guarantees that it will
                         * handle values outside its set gracefully and falls back
                         * to a default value when unknown.
                         *
                         * If this property is not present the client only supports
                         * the completion items kinds from `Text` to `Reference` as defined in
                         * the initial version of the protocol.
                         */
                        valueSet?: CompletionItemKind[];
                },

                /**
                 * The client supports to send additional context information for a
                 * `textDocument/completion` request.
                 */
                contextSupport?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/hover`
         */
        hover?: {
                /**
                 * Whether hover supports dynamic registration.
                 */
                dynamicRegistration?: boolean;

                /**
                 * Client supports the follow content formats for the content
                 * property. The order describes the preferred format of the client.
                 */
                contentFormat?: MarkupKind[];
        };

        /**
         * Capabilities specific to the `textDocument/signatureHelp`
         */
        signatureHelp?: {
                /**
                 * Whether signature help supports dynamic registration.
                 */
                dynamicRegistration?: boolean;

                /**
                 * The client supports the following `SignatureInformation`
                 * specific properties.
                 */
                signatureInformation?: {
                        /**
                         * Client supports the follow content formats for the documentation
                         * property. The order describes the preferred format of the client.
                         */
                        documentationFormat?: MarkupKind[];
                };
        };

        /**
         * Capabilities specific to the `textDocument/references`
         */
        references?: {
                /**
                 * Whether references supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/documentHighlight`
         */
        documentHighlight?: {
                /**
                 * Whether document highlight supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/documentSymbol`
         */
        documentSymbol?: {
                /**
                 * Whether document symbol supports dynamic registration.
                 */
                dynamicRegistration?: boolean;

                /**
                 * Specific capabilities for the `SymbolKind`.
                 */
                symbolKind?: {
                        /**
                         * The symbol kind values the client supports. When this
                         * property exists the client also guarantees that it will
                         * handle values outside its set gracefully and falls back
                         * to a default value when unknown.
                         *
                         * If this property is not present the client only supports
                         * the symbol kinds from `File` to `Array` as defined in
                         * the initial version of the protocol.
                         */
                        valueSet?: SymbolKind[];
                }
        };

        /**
         * Capabilities specific to the `textDocument/formatting`
         */
        formatting?: {
                /**
                 * Whether formatting supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/rangeFormatting`
         */
        rangeFormatting?: {
                /**
                 * Whether range formatting supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/onTypeFormatting`
         */
        onTypeFormatting?: {
                /**
                 * Whether on type formatting supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/definition`
         */
        definition?: {
                /**
                 * Whether definition supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/typeDefinition`
         *
         * Since 3.6.0
         */
        typeDefinition?: {
                /**
                 * Whether typeDefinition supports dynamic registration. If this is set to `true`
                 * the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
                 * return value for the corresponding server capability as well.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/implementation`.
         *
         * Since 3.6.0
         */
        implementation?: {
                /**
                 * Whether implementation supports dynamic registration. If this is set to `true`
                 * the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
                 * return value for the corresponding server capability as well.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/codeAction`
         */
        codeAction?: {
                /**
                 * Whether code action supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
                /**
                 * The client support code action literals as a valid
                 * response of the `textDocument/codeAction` request.
                 *
                 * Since 3.8.0
                 */
                codeActionLiteralSupport?: {
                        /**
                         * The code action kind is support with the following value
                         * set.
                         */
                        codeActionKind: {

                                /**
                                 * The code action kind values the client supports. When this
                                 * property exists the client also guarantees that it will
                                 * handle values outside its set gracefully and falls back
                                 * to a default value when unknown.
                                 */
                                valueSet: CodeActionKind[];
                        };
                };
        };

        /**
         * Capabilities specific to the `textDocument/codeLens`
         */
        codeLens?: {
                /**
                 * Whether code lens supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/documentLink`
         */
        documentLink?: {
                /**
                 * Whether document link supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/documentColor` and the
         * `textDocument/colorPresentation` request.
         *
         * Since 3.6.0
         */
        colorProvider?: {
                /**
                 * Whether colorProvider supports dynamic registration. If this is set to `true`
                 * the client supports the new `(ColorProviderOptions & TextDocumentRegistrationOptions & StaticRegistrationOptions)`
                 * return value for the corresponding server capability as well.
                 */
                dynamicRegistration?: boolean;
        }

        /**
         * Capabilities specific to the `textDocument/rename`
         */
        rename?: {
                /**
                 * Whether rename supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to `textDocument/publishDiagnostics`.
         */
        publishDiagnostics?: {
                /**
                 * Whether the clients accepts diagnostics with related information.
                 */
                relatedInformation?: boolean;
        };

        /**
	 * Capabilities specific to `textDocument/foldingRange` requests.
	 *
	 * Since 3.10.0
	 */
	foldingRange?: {
		/**
		 * Whether implementation supports dynamic registration for folding range providers. If this is set to `true`
		 * the client supports the new `(FoldingRangeProviderOptions & TextDocumentRegistrationOptions & StaticRegistrationOptions)`
		 * return value for the corresponding server capability as well.
		 */
		dynamicRegistration?: boolean;
		/**
		 * The maximum number of folding ranges that the client prefers to receive per document. The value serves as a
		 * hint, servers are free to follow the limit.
		 */
		rangeLimit?: number;
		/**
		 * If set, the client signals that it only supports folding complete lines. If set, client will
		 * ignore specified `startCharacter` and `endCharacter` properties in a FoldingRange.
		 */
		lineFoldingOnly?: boolean;
	};
}

-}

-- -------------------------------------

-- TODO:AZ: this name is Java-ridiculously long
data SynchronizationTextDocumentClientCapabilities =
  SynchronizationTextDocumentClientCapabilities
    { -- | Whether text document synchronization supports dynamic registration.
      _synchronizationTextDocumentClientCapabilitiesDynamicRegistration :: Maybe Bool

      -- | The client supports sending will save notifications.
    , _synchronizationTextDocumentClientCapabilitiesWillSave :: Maybe Bool

      -- | The client supports sending a will save request and waits for a
      -- response providing text edits which will be applied to the document
      -- before it is saved.
    , _synchronizationTextDocumentClientCapabilitiesWillSaveWaitUntil :: Maybe Bool

      -- | The client supports did save notifications.
    , _synchronizationTextDocumentClientCapabilitiesDidSave :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''SynchronizationTextDocumentClientCapabilities)

instance Default SynchronizationTextDocumentClientCapabilities where
  def = SynchronizationTextDocumentClientCapabilities def def def def

-- -------------------------------------

data CompletionItemClientCapabilities =
  CompletionItemClientCapabilities
    { -- | Client supports snippets as insert text.
      --
      -- A snippet can define tab stops and placeholders with `$1`, `$2` and
      -- `${3:foo}`. `$0` defines the final tab stop, it defaults to the end of
      -- the snippet. Placeholders with equal identifiers are linked, that is
      -- typing in one will update others too.
      _completionItemClientCapabilitiesSnippetSupport :: Maybe Bool

      -- | Client supports commit characters on a completion item.
    , _completionItemClientCapabilitiesCommitCharactersSupport :: Maybe Bool

      -- | Client supports the follow content formats for the documentation
      -- property. The order describes the preferred format of the client.
    , _completionItemClientCapabilitiesDocumentationFormat :: Maybe (List MarkupKind)

      -- | Client supports the deprecated property on a completion item.
    , _completionItemClientCapabilitiesDeprecatedSupport :: Maybe Bool

      -- | Client supports the preselect property on a completion item.
    , _completionItemClientCapabilitiesPreselectSupport :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''CompletionItemClientCapabilities)

data CompletionItemKindClientCapabilities =
  CompletionItemKindClientCapabilities
    { -- | The completion item kind values the client supports. When this
      -- property exists the client also guarantees that it will
      --  handle values outside its set gracefully and falls back
      --  to a default value when unknown.
      _completionItemKindClientCapabilitiesValueSet :: Maybe (List CompletionItemKind)
    }
  deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''CompletionItemKindClientCapabilities)

data CompletionClientCapabilities =
  CompletionClientCapabilities
    { _completionClientCapabilitiesDynamicRegistration :: Maybe Bool -- ^Whether completion supports dynamic
                                         -- registration.
    , _completionClientCapabilitiesCompletionItem :: Maybe CompletionItemClientCapabilities
    , _completionClientCapabilitiesCompletionItemKind :: Maybe CompletionItemKindClientCapabilities
    , _completionClientCapabilitiesContextSupport :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''CompletionClientCapabilities)

-- -------------------------------------

data HoverClientCapabilities =
  HoverClientCapabilities
    { _hoverClientCapabilitiesDynamicRegistration :: Maybe Bool
    , _hoverClientCapabilitiesContentFormat :: Maybe (List MarkupKind)
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''HoverClientCapabilities)

-- -------------------------------------

data SignatureInformationClientCapabilities =
  SignatureInformationClientCapabilities
    { -- | Client supports the follow content formats for the documentation
      -- property. The order describes the preferred format of the client.
      signatureInformationClientCapabilitiesDocumentationFormat :: Maybe (List MarkupKind)
    }
  deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''SignatureInformationClientCapabilities)

data SignatureHelpClientCapabilities =
  SignatureHelpClientCapabilities
    { -- | Whether signature help supports dynamic registration.
      _signatureHelpClientCapabilitiesDynamicRegistration :: Maybe Bool

      -- | The client supports the following `SignatureInformation`
      -- specific properties.
    , _signatureHelpClientCapabilitiesSignatureInformation :: Maybe SignatureInformationClientCapabilities
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''SignatureHelpClientCapabilities)

-- -------------------------------------

data ReferencesClientCapabilities =
  ReferencesClientCapabilities
    { _referencesClientCapabilitiesDynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''ReferencesClientCapabilities)

-- -------------------------------------

data DocumentHighlightClientCapabilities =
  DocumentHighlightClientCapabilities
    { _documentHighlightClientCapabilitiesDynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''DocumentHighlightClientCapabilities)

-- -------------------------------------

data DocumentSymbolKindClientCapabilities =
  DocumentSymbolKindClientCapabilities
    { -- | The symbol kind values the client supports. When this
      --  property exists the client also guarantees that it will
      --  handle values outside its set gracefully and falls back
      --  to a default value when unknown.
      --
      --  If this property is not present the client only supports
      --  the symbol kinds from `File` to `Array` as defined in
      --  the initial version of the protocol.
      _documentSymbolKindClientCapabilitiesValueSet :: Maybe (List SymbolKind)
    }
  deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''DocumentSymbolKindClientCapabilities)

data DocumentSymbolClientCapabilities =
  DocumentSymbolClientCapabilities
    { -- | Whether document symbol supports dynamic registration.
      _documentSymbolClientCapabilitiesDynamicRegistration :: Maybe Bool
      -- | Specific capabilities for the `SymbolKind`.
    , _documentSymbolClientCapabilitiesSymbolKind :: Maybe DocumentSymbolKindClientCapabilities
    , _documentSymbolClientCapabilitiesHierarchicalDocumentSymbolSupport :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''DocumentSymbolClientCapabilities)

-- -------------------------------------

data FormattingClientCapabilities =
  FormattingClientCapabilities
    { _formattingClientCapabilitiesDynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''FormattingClientCapabilities)

-- -------------------------------------

data RangeFormattingClientCapabilities =
  RangeFormattingClientCapabilities
    { _rangeFormattingClientCapabilitiesDynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''RangeFormattingClientCapabilities)

-- -------------------------------------

data OnTypeFormattingClientCapabilities =
  OnTypeFormattingClientCapabilities
    { _onTypeFormattingClientCapabilitiesDynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''OnTypeFormattingClientCapabilities)

-- -------------------------------------

data DefinitionClientCapabilities =
  DefinitionClientCapabilities
    { _definitionClientCapabilitiesDynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''DefinitionClientCapabilities)

-- -------------------------------------

data TypeDefinitionClientCapabilities =
  TypeDefinitionClientCapabilities
    { -- | Whether typeDefinition supports dynamic registration. If this is set to `true`
      --  the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
      --  return value for the corresponding server capability as well.
      _typeDefinitionClientCapabilitiesDynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''TypeDefinitionClientCapabilities)

-- -------------------------------------
--
data ImplementationClientCapabilities =
  ImplementationClientCapabilities
    { -- | Whether implementation supports dynamic registration. If this is set to `true`
      -- the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
      -- return value for the corresponding server capability as well.
      _implementationClientCapabilitiesDynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''ImplementationClientCapabilities)

-- -------------------------------------

data CodeActionKindClientCapabilities =
  CodeActionKindClientCapabilities
   { -- | The code action kind values the client supports. When this
     -- property exists the client also guarantees that it will
     -- handle values outside its set gracefully and falls back
     -- to a default value when unknown.
      _codeActionKindClientCapabilitiesValueSet :: List CodeActionKind
   } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''CodeActionKindClientCapabilities)

instance Default CodeActionKindClientCapabilities where
  def = CodeActionKindClientCapabilities (List allKinds)
    where allKinds = [ CodeActionQuickFix
                     , CodeActionRefactor
                     , CodeActionRefactorExtract
                     , CodeActionRefactorInline
                     , CodeActionRefactorRewrite
                     , CodeActionSource
                     , CodeActionSourceOrganizeImports
                     ]

data CodeActionLiteralSupport =
  CodeActionLiteralSupport
    { _codeActionLiteralSupportCodeActionKind :: CodeActionKindClientCapabilities -- ^ The code action kind is support with the following value set.
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''CodeActionLiteralSupport)

data CodeActionClientCapabilities =
  CodeActionClientCapabilities
    { _codeActionClientCapabilitiesDynamicRegistration      :: Maybe Bool -- ^ Whether code action supports dynamic registration.
    , _codeActionClientCapabilitiesCodeActionLiteralSupport :: Maybe CodeActionLiteralSupport -- ^ The client support code action literals as a valid response
                                                                  -- of the `textDocument/codeAction` request.
                                                                  -- Since 3.8.0
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''CodeActionClientCapabilities)

-- -------------------------------------

data CodeLensClientCapabilities =
  CodeLensClientCapabilities
    { _codeLensClientCapabilitiesDynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''CodeLensClientCapabilities)

-- -------------------------------------

data DocumentLinkClientCapabilities =
  DocumentLinkClientCapabilities
    { _documentLinkClientCapabilitiesDynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''DocumentLinkClientCapabilities)

-- -------------------------------------

data ColorProviderClientCapabilities =
  ColorProviderClientCapabilities
    { -- | Whether colorProvider supports dynamic registration. If this is set to `true`
      --  the client supports the new `(ColorProviderOptions & TextDocumentRegistrationOptions & StaticRegistrationOptions)`
      --  return value for the corresponding server capability as well.
      _colorProviderClientCapabilitiesDynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''ColorProviderClientCapabilities)

-- -------------------------------------

data RenameClientCapabilities =
  RenameClientCapabilities
    { _renameClientCapabilitiesDynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''RenameClientCapabilities)

-- -------------------------------------

data PublishDiagnosticsClientCapabilities =
  PublishDiagnosticsClientCapabilities
    { -- | Whether the clients accepts diagnostics with related information.
      _publishDiagnosticsClientCapabilitiesRelatedInformation :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''PublishDiagnosticsClientCapabilities)

-- -------------------------------------

data FoldingRangeClientCapabilities =
  FoldingRangeClientCapabilities
    { -- | Whether implementation supports dynamic registration for folding range
      -- providers. If this is set to `true` the client supports the new
      -- `(FoldingRangeProviderOptions & TextDocumentRegistrationOptions & StaticRegistrationOptions)`
      -- return value for the corresponding server capability as well.
      _foldingRangeClientCapabilitiesDynamicRegistration :: Maybe Bool
      -- | The maximum number of folding ranges that the client prefers to receive
      -- per document. The value serves as a hint, servers are free to follow the limit.
    , _foldingRangeClientCapabilitiesRangeLimit          :: Maybe Int
      -- | If set, the client signals that it only supports folding complete lines. If set,
      -- client will ignore specified `startCharacter` and `endCharacter` properties in a
      -- FoldingRange.
    , _foldingRangeClientCapabilitiesLineFoldingOnly     :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''FoldingRangeClientCapabilities)

-- -------------------------------------

data TextDocumentClientCapabilities =
  TextDocumentClientCapabilities
    { _textDocumentClientCapabilitiesSynchronization :: Maybe SynchronizationTextDocumentClientCapabilities

      -- | Capabilities specific to the `textDocument/completion`
    , _textDocumentClientCapabilitiesCompletion :: Maybe CompletionClientCapabilities

      -- | Capabilities specific to the `textDocument/hover`
    , _textDocumentClientCapabilitiesHover :: Maybe HoverClientCapabilities

      -- | Capabilities specific to the `textDocument/signatureHelp`
    , _textDocumentClientCapabilitiesSignatureHelp :: Maybe SignatureHelpClientCapabilities

      -- | Capabilities specific to the `textDocument/references`
    , _textDocumentClientCapabilitiesReferences :: Maybe ReferencesClientCapabilities

      -- | Capabilities specific to the `textDocument/documentHighlight`
    , _textDocumentClientCapabilitiesDocumentHighlight :: Maybe DocumentHighlightClientCapabilities

      -- | Capabilities specific to the `textDocument/documentSymbol`
    , _textDocumentClientCapabilitiesDocumentSymbol :: Maybe DocumentSymbolClientCapabilities

      -- | Capabilities specific to the `textDocument/formatting`
    , _textDocumentClientCapabilitiesFormatting :: Maybe FormattingClientCapabilities

      -- | Capabilities specific to the `textDocument/rangeFormatting`
    , _textDocumentClientCapabilitiesRangeFormatting :: Maybe RangeFormattingClientCapabilities

      -- | Capabilities specific to the `textDocument/onTypeFormatting`
    , _textDocumentClientCapabilitiesOnTypeFormatting :: Maybe OnTypeFormattingClientCapabilities

      -- | Capabilities specific to the `textDocument/definition`
    , _textDocumentClientCapabilitiesDefinition :: Maybe DefinitionClientCapabilities

      -- | Capabilities specific to the `textDocument/typeDefinition`
    , _textDocumentClientCapabilitiesTypeDefinition :: Maybe TypeDefinitionClientCapabilities

      -- | Capabilities specific to the `textDocument/implementation`
    , _textDocumentClientCapabilitiesImplementation :: Maybe ImplementationClientCapabilities

      -- | Capabilities specific to the `textDocument/codeAction`
    , _textDocumentClientCapabilitiesCodeAction :: Maybe CodeActionClientCapabilities

      -- | Capabilities specific to the `textDocument/codeLens`
    , _textDocumentClientCapabilitiesCodeLens :: Maybe CodeLensClientCapabilities

      -- | Capabilities specific to the `textDocument/documentLink`
    , _textDocumentClientCapabilitiesDocumentLink :: Maybe DocumentLinkClientCapabilities

      -- | Capabilities specific to the `textDocument/documentColor` and the
      -- `textDocument/colorPresentation` request
    , _textDocumentClientCapabilitiesColorProvider :: Maybe ColorProviderClientCapabilities

      -- | Capabilities specific to the `textDocument/rename`
    , _textDocumentClientCapabilitiesRename :: Maybe RenameClientCapabilities

      -- | Capabilities specific to `textDocument/publishDiagnostics`
    , _textDocumentClientCapabilitiesPublishDiagnostics :: Maybe PublishDiagnosticsClientCapabilities

      -- | Capabilities specific to `textDocument/foldingRange` requests. Since LSP 3.10.
      --
      -- @since 0.7.0.0
    , _textDocumentClientCapabilitiesFoldingRange :: Maybe FoldingRangeClientCapabilities
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''TextDocumentClientCapabilities)

instance Default TextDocumentClientCapabilities where
  def = TextDocumentClientCapabilities def def def def def def def def
                                       def def def def def def def def
                                       def def def def

-- ---------------------------------------------------------------------

-- | Window specific client capabilities.
data WindowClientCapabilities =
  WindowClientCapabilities
    { -- | Whether client supports handling progress notifications.
      _windowClientCapabilitiesProgress :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''WindowClientCapabilities)

instance Default WindowClientCapabilities where
  def = WindowClientCapabilities def

-- ---------------------------------------------------------------------
{-
New in 3.0

-----------

ClientCapabilities

now define capabilities for dynamic registration, workspace and text document
features the client supports. The experimental can be used to pass experimential
capabilities under development. For future compatibility a ClientCapabilities
object literal can have more properties set than currently defined. Servers
receiving a ClientCapabilities object literal with unknown properties should
ignore these properties. A missing property should be interpreted as an absence
of the capability. If a property is missing that defines sub properties all sub
properties should be interpreted as an absence of the capability.

Client capabilities got introduced with the version 3.0 of the protocol. They
therefore only describe capabilities that got introduced in 3.x or later.
Capabilities that existed in the 2.x version of the protocol are still mandatory
for clients. Clients cannot opt out of providing them. So even if a client omits
the ClientCapabilities.textDocument.synchronization it is still required that
the client provides text document synchronization (e.g. open, changed and close
notifications).

interface ClientCapabilities {
        /**
         * Workspace specific client capabilities.
         */
        workspace?: WorkspaceClientCapabilities;

        /**
         * Text document specific client capabilities.
         */
        textDocument?: TextDocumentClientCapabilities;

        /**
         * Experimental client capabilities.
         */
        experimental?: any;

        /**
         * Window specific client capabilities.
	       */
	      window?: WindowClientCapabilities;
}
-}

data ClientCapabilities =
  ClientCapabilities
    { _clientCapabilitiesWorkspace    :: Maybe WorkspaceClientCapabilities
    , _clientCapabilitiesTextDocument :: Maybe TextDocumentClientCapabilities
    -- | Capabilities specific to `window/progress` requests. Experimental.
    --
    -- @since 0.10.0.0
    , _clientCapabilitiesWindow :: Maybe WindowClientCapabilities
    , _clientCapabilitiesExperimental :: Maybe A.Object
    } deriving (Show, Read, Eq)

$(deriveLspJSON lspOptions ''ClientCapabilities)

instance Default ClientCapabilities where
  def = ClientCapabilities def def def def
