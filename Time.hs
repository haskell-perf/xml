{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.DeepSeq
import           Control.Monad.ST
import           Criterion.Main
import           Criterion.Types
import           Data.Book
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy
import qualified Data.Conduit
import qualified Data.Conduit.List
import           Data.Default
import           Data.FileEmbed
import           Data.STRef
import           GHC.Generics (Generic)
import qualified SAX
import qualified SAX.Streaming
import qualified Streaming.Prelude
import qualified Text.XML
import qualified Text.XML.DOM.Parser
import qualified Text.XML.Expat.SAX
import qualified Text.XML.Expat.Tree
import qualified Text.XML.Hexml
import qualified Text.XML.Stream.Parse
import qualified Xeno.DOM
import qualified Xeno.SAX
import qualified Xmlbf
import qualified Xmlbf.Xeno
#ifdef LIBXML
import qualified Data.XML.Types
import qualified Text.XML.LibXML
import qualified Text.XML.LibXML.SAX
#endif

data XenoEvent
  = OpenTag ByteString
  | EndOpenTag ByteString
  | CloseTag ByteString
  | Attribute ByteString ByteString
  | Text ByteString
  | CDATA ByteString
  deriving (Generic)

instance NFData XenoEvent

main :: IO ()
main = defaultMainWith
  defaultConfig { csvFile = Just "out.csv" }
  [ bgroup "bs-sax" (bsSax inputBs)
  , bgroup "bs-dom" (bsDom inputBs)
  , bgroup "dom-struct" (domStruct inputBs)
  , bgroup "bs-struct" (bsStruct inputBs)
  ]

-- | Conversion from 'ByteString' to a list of SAX events
bsSax :: ByteString -> [Benchmark]
bsSax bs =
  [ bench "xeno fold list" $ nf
    ( Xeno.SAX.fold
      (\v   -> (:v) . OpenTag)
      (\v k -> (:v) . Attribute k)
      (\v   -> (:v) . EndOpenTag)
      (\v   -> (:v) . Text)
      (\v   -> (:v) . CloseTag)
      (\v   -> (:v) . CDATA)
      mempty )
    bs
  , bench "xeno process STRef" $ nf
    ( \input -> runST $ do
        ref <- newSTRef []
        let add e = modifySTRef ref (e:)
        Xeno.SAX.process
          (add . OpenTag)
          (\k -> add . Attribute k)
          (add . EndOpenTag)
          (add . Text)
          (add . CloseTag)
          (add . CDATA)
          input
        readSTRef ref )
    bs
  , bench "xeno process conduit" $ nf
    ( \input ->
        let
          parseBs = Xeno.SAX.process
            (Data.Conduit.yield . OpenTag)
            (\k -> Data.Conduit.yield . Attribute k)
            (Data.Conduit.yield . EndOpenTag)
            (Data.Conduit.yield . Text)
            (Data.Conduit.yield . CloseTag)
            (Data.Conduit.yield . CDATA)
            input
        in case Data.Conduit.connect parseBs Data.Conduit.List.consume of
          Nothing -> error "Unexpected parse error"
          Just v  -> v )
    bs
  , bench "sax" $ nf
    ( \input -> case Streaming.Prelude.toList_ (SAX.Streaming.streamXml input) of
        Left _  -> error "Unexpected parse error"
        Right v -> v )
    bs
  , bench "hexpat" $ nf
    ( Text.XML.Expat.SAX.parse @ByteString @ByteString Text.XML.Expat.SAX.defaultParseOptions )
    ( Data.ByteString.Lazy.fromStrict bs )
#ifdef LIBXML
  , bench "libxml" $ nf
    ( \input -> runST $ do
        ref <- newSTRef []
        p <- Text.XML.LibXML.SAX.newParserST Nothing
        let
          add e     = modifySTRef ref (e:) >> return True
          set cb st = Text.XML.LibXML.SAX.setCallback p cb st
        set Text.XML.LibXML.SAX.parsedBeginElement (\n -> add . Data.XML.Types.EventBeginElement n)
        set Text.XML.LibXML.SAX.parsedEndElement (add . Data.XML.Types.EventEndElement)
        set Text.XML.LibXML.SAX.parsedCharacters (add . Data.XML.Types.EventContent . Data.XML.Types.ContentText)
        set Text.XML.LibXML.SAX.parsedCDATA (add . Data.XML.Types.EventCDATA)
        Text.XML.LibXML.SAX.parseBytes p input
        readSTRef ref )
    bs
#endif
  , bench "conduit" $ nf
    ( \input -> case Data.Conduit.connect (Text.XML.Stream.Parse.parseLBS def input) Data.Conduit.List.consume of
        Nothing -> error "Unexpected parse error"
        Just v  -> v )
    ( Data.ByteString.Lazy.fromStrict bs )
  ]

-- | Conversion from 'ByteString' to DOM
bsDom :: ByteString -> [Benchmark]
bsDom bs =
  [ bench "hexml" $ whnf
    ( \input -> case Text.XML.Hexml.parse input of
        Left _  -> error "Unexpected parse error"
        Right v -> v )
    bs
  , bench "xeno" $ nf
    ( \input -> case Xeno.DOM.parse input of
        Left _  -> error "Unexpected parse error"
        Right v -> v )
    bs
#ifdef LIBXML
  , bench "libxml" $ whnfIO (Text.XML.LibXML.parseMemory bs)
#endif
  , bench "hexpat" $ nf
    ( \input -> case Text.XML.Expat.Tree.parse' @ByteString @ByteString Text.XML.Expat.Tree.defaultParseOptions input of
        Left _  -> error "Unexpected parse error"
        Right v -> v )
    bs
  , bench "xml-conduit" $ nf
    ( Text.XML.parseLBS_ def )
    ( Data.ByteString.Lazy.fromStrict bs )
  ]

-- | Conversion from DOM to data type
domStruct :: ByteString -> [Benchmark]
domStruct bs =
  [ bench "sax" $ nf
    ( \stream -> case SAX.parseSax rootSaxParser stream of
        SAX.Partial _ _ _ -> error "Unexpected conversion error: Partial"
        SAX.Done r        -> r
        SAX.Fail e        -> error $ "Unexpected conversion error: Fail " ++ e )
    ( SAX.Streaming.streamXml bs )
  , bench "xmlbf-xeno" $ nf
    ( \case
        Left _  -> error "Unexpected parse error"
        Right n -> case Xmlbf.Xeno.element n of
          Left e     -> error e
          Right node -> case Xmlbf.runParser (Xmlbf.fromXml @Root) [node] of
            Left e  -> error e
            Right v -> v )
    ( Xeno.DOM.parse bs )
  , bench "dom-parser" $ nf
    ( \doc -> case Text.XML.DOM.Parser.runDomParser doc (Text.XML.DOM.Parser.inElem "catalog" $ Text.XML.DOM.Parser.fromDom @Catalog) of
        Left e  -> error $ "Unexpected conversion error: " ++ show e
        Right v -> v )
    ( Text.XML.parseLBS_ def (Data.ByteString.Lazy.fromStrict bs) )
  ]

-- | Conversion from 'ByteString' to data type
bsStruct :: ByteString -> [Benchmark]
bsStruct bs =
  [ bench "sax" $ nf
    ( \input -> case SAX.parseSax rootSaxParser (SAX.Streaming.streamXml input) of
        SAX.Partial _ _ _ -> error "Unexpected conversion error: Partial"
        SAX.Done r        -> r
        SAX.Fail e        -> error $ "Unexpected conversion error: Fail " ++ e )
    bs
  , bench "xmlbf-xeno" $ nf
    ( \input -> case Xeno.DOM.parse input of
        Left _  -> error "Unexpected parse error"
        Right n -> case Xmlbf.Xeno.element n of
          Left e     -> error e
          Right node -> case Xmlbf.runParser (Xmlbf.fromXml @Root) [node] of
            Left e  -> error e
            Right v -> v )
    bs
  , bench "dom-parser" $ nf
    ( \input -> case Text.XML.DOM.Parser.runDomParser (Text.XML.parseLBS_ def input) (Text.XML.DOM.Parser.inElem "catalog" $ Text.XML.DOM.Parser.fromDom @Catalog) of
        Left e  -> error $ "Unexpected conversion error: " ++ show e
        Right v -> v )
    ( Data.ByteString.Lazy.fromStrict bs )
  ]

inputBs :: ByteString
inputBs = $(embedFile "in.xml")

-- Assuming that SAX representation is strict (NF=WHNF)
instance NFData SAX.Streaming.SaxEvent where rnf = rwhnf
