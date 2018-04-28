{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Control.DeepSeq
import qualified Data.Book
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy
import           Data.Default
import           Data.FileEmbed
import qualified Foreign.ForeignPtr
import qualified SAX
import qualified SAX.Streaming
import qualified Text.XML
import qualified Text.XML.DOM.Parser
import qualified Text.XML.Expat.Tree
import qualified Text.XML.Hexml
import           Weigh
import qualified Xeno.DOM
import qualified Xmlbf
import qualified Xmlbf.Xeno
#ifdef LIBXML
import qualified Text.XML.LibXML
#endif

main :: IO ()
main = mainWith $ do
  bsDom inputBs
  domStruct inputBs
  bsStruct inputBs

-- | Conversion from 'ByteString' to DOM
bsDom :: ByteString -> Weigh ()
bsDom bs = do
#ifdef LIBXML
  io "libxml" Text.XML.LibXML.parseMemory bs
#endif
  func "hexml"
    ( \input -> case Text.XML.Hexml.parse input of
        Left _  -> error "Unexpected parse error"
        Right v -> v )
    bs
  func "xeno"
    ( \input -> case Xeno.DOM.parse input of
        Left _  -> error "Unexpected parse error"
        Right v -> v )
    bs
  func "hexpat"
    ( \input -> case Text.XML.Expat.Tree.parse' @ByteString @ByteString Text.XML.Expat.Tree.defaultParseOptions input of
        Left _  -> error "Unexpected parse error"
        Right v -> v )
    bs
  func "xml-conduit"
    ( Text.XML.parseLBS_ def )
    ( Data.ByteString.Lazy.fromStrict bs )

-- | Conversion from DOM to data type
domStruct :: ByteString -> Weigh ()
domStruct bs = do
  func "dom/sax"
    ( \stream -> case SAX.parseSax Data.Book.rootSaxParser stream of
        SAX.Partial _ _ _ -> error "Unexpected conversion error: Partial"
        SAX.Done r        -> r
        SAX.Fail e        -> error $ "Unexpected conversion error: Fail " ++ e )
    ( SAX.Streaming.streamXml bs )
  func "dom/xmlbf-xeno"
    ( \case
        Left _  -> error "Unexpected parse error"
        Right n -> case Xmlbf.Xeno.element n of
          Left e     -> error e
          Right node -> case Xmlbf.runParser (Xmlbf.fromXml @Data.Book.Root) [node] of
            Left e  -> error e
            Right v -> v )
    ( Xeno.DOM.parse bs )
  func "dom/dom-parser"
    ( \doc -> case Text.XML.DOM.Parser.runDomParser doc (Text.XML.DOM.Parser.inElem "catalog" $ Text.XML.DOM.Parser.fromDom @Data.Book.Catalog) of
        Left e  -> error $ "Unexpected conversion error: " ++ show e
        Right v -> v )
    ( Text.XML.parseLBS_ def (Data.ByteString.Lazy.fromStrict bs) )

-- | Conversion from 'ByteString' to data type
bsStruct :: ByteString -> Weigh ()
bsStruct bs = do
  func "bs/sax"
    ( \input -> case SAX.parseSax Data.Book.rootSaxParser (SAX.Streaming.streamXml input) of
        SAX.Partial _ _ _ -> error "Unexpected conversion error: Partial"
        SAX.Done r        -> r
        SAX.Fail e        -> error $ "Unexpected conversion error: Fail " ++ e )
    bs
  func "bs/xmlbf-xeno"
    ( \input -> case Xeno.DOM.parse input of
        Left _  -> error "Unexpected parse error"
        Right n -> case Xmlbf.Xeno.element n of
          Left e     -> error e
          Right node -> case Xmlbf.runParser (Xmlbf.fromXml @Data.Book.Root) [node] of
            Left e  -> error e
            Right v -> v )
    bs
  func "bs/dom-parser"
    ( \input -> case Text.XML.DOM.Parser.runDomParser (Text.XML.parseLBS_ def input) (Text.XML.DOM.Parser.inElem "catalog" $ Text.XML.DOM.Parser.fromDom @Data.Book.Catalog) of
        Left e  -> error $ "Unexpected conversion error: " ++ show e
        Right v -> v )
    ( Data.ByteString.Lazy.fromStrict bs )

inputBs :: Data.ByteString.ByteString
inputBs = $(embedFile "in.xml")

-- Assuming that DOM representation is strict (NF=WHNF)
instance NFData Text.XML.Hexml.Node where rnf = rwhnf
instance NFData (Foreign.ForeignPtr.ForeignPtr a) where rnf = rwhnf
#ifdef LIBXML
instance NFData Text.XML.LibXML.Document where rnf = rwhnf
#endif
