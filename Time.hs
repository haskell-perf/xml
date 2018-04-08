{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Criterion.Main
import           Criterion.Types
import           Data.Book
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import           Data.Default
import           Data.FileEmbed
import qualified Text.XML
import qualified Text.XML.DOM.Parser
import qualified Text.XML.Hexml
import qualified Xeno.DOM
import qualified Xmlbf
import qualified Xmlbf.Xeno

main :: IO ()
main = defaultMainWith
  defaultConfig { csvFile = Just "out.csv" }
  [ bgroup "dom" (dom inputBs)
  , bgroup "struct" (struct inputBs)
  ]

-- | Conversion from 'Data.ByteString.ByteString' to DOM
dom :: Data.ByteString.ByteString -> [Benchmark]
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
  , bench "xml-conduit" $ nf
    ( \input -> Text.XML.parseLBS_ def input )
    ( Data.ByteString.Lazy.fromStrict bs )
  ]

-- | Conversion from DOM to data type
struct :: Data.ByteString.ByteString -> [Benchmark]
struct bs =
  [ bench "dom-parser" $ nf
    ( \doc -> case Text.XML.DOM.Parser.runDomParser doc (Text.XML.DOM.Parser.fromDom @Catalog) of
        Left _  -> error "Unexpected conversion error"
        Right v -> v )
    ( Text.XML.parseLBS_ def (Data.ByteString.Lazy.fromStrict bs) )
  -- TODO: https://gitlab.com/k0001/xmlbf/issues/6
  , bench "xmlbf-xeno" $ nf
    ( \case
        Left _  -> error "Unexpected parse error"
        Right n -> case Xmlbf.Xeno.element n of
          Left e     -> error e
          Right node -> case Xmlbf.runParser (Xmlbf.fromXml @Root) [node] of
            Left e  -> error e
            Right v -> v )
    ( Xeno.DOM.parse inputBs )
  ]

inputBs :: Data.ByteString.ByteString
inputBs = $(embedFile "in.xml")
