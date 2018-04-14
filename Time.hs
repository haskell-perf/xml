{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

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
import qualified Text.XML.LibXML
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
  [ bgroup "sax" (sax inputBs)
  , bgroup "dom" (dom inputBs)
  , bgroup "struct" (struct inputBs)
  ]

-- | Conversion from 'ByteString' to a list of SAX events
sax :: ByteString -> [Benchmark]
sax bs =
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
  , bench "conduit" $ nf
    ( \input -> case Data.Conduit.connect (Text.XML.Stream.Parse.parseLBS def input) Data.Conduit.List.consume of
        Nothing -> error "Unexpected parse error"
        Just v  -> v )
    ( Data.ByteString.Lazy.fromStrict bs )
  ]

-- | Conversion from 'ByteString' to DOM
dom :: ByteString -> [Benchmark]
dom bs =
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
struct :: ByteString -> [Benchmark]
struct bs =
  [ bench "xmlbf-xeno" $ nf
    ( \case
        Left _  -> error "Unexpected parse error"
        Right n -> case Xmlbf.Xeno.element n of
          Left e     -> error e
          Right node -> case Xmlbf.runParser (Xmlbf.fromXml @Root) [node] of
            Left e  -> error e
            Right v -> v )
    ( Xeno.DOM.parse inputBs )
  -- TODO: Fix @SAX.Fail "fail handler"@ error
  -- , bench "sax" $ nf
  --   ( \stream -> case SAX.parseSax rootSaxParser stream of
  --       SAX.Partial _ _ _ -> error "Unexpected conversion error: Partial"
  --       SAX.Done r        -> r
  --       SAX.Fail e        -> error $ "Unexpected conversion error: Fail " ++ e )
  --   ( SAX.Streaming.streamXml bs )
  , bench "dom-parser" $ nf
    ( \doc -> case Text.XML.DOM.Parser.runDomParser doc (Text.XML.DOM.Parser.fromDom @Catalog) of
        Left _  -> error "Unexpected conversion error"
        Right v -> v )
    ( Text.XML.parseLBS_ def (Data.ByteString.Lazy.fromStrict bs) )
  ]

inputBs :: ByteString
inputBs = $(embedFile "in.xml")

-- Assuming that SAX representation is strict (NF=WHNF)
instance NFData SAX.Streaming.SaxEvent where rnf = rwhnf
