{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Control.DeepSeq
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import           Data.Default
import           Data.FileEmbed
import qualified Foreign.ForeignPtr
import qualified Text.XML
import qualified Text.XML.Hexml
import           Weigh
import qualified Xeno.DOM

main :: IO ()
main = mainWith $ do
  setColumns [Case, Allocated, Max, Live, GCs]
  dom medium

dom :: Data.ByteString.ByteString -> Weigh ()
dom bs = do
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
  func "xml-conduit"
    ( \input -> Text.XML.parseLBS_ def input )
    ( Data.ByteString.Lazy.fromStrict bs )

medium :: Data.ByteString.ByteString
medium = $(embedFile "data/fabricated-211kb.xml")

-- Assuming that hexml DOM is strict (NF=WHNF)
instance NFData Text.XML.Hexml.Node where rnf = rwhnf
instance NFData (Foreign.ForeignPtr.ForeignPtr a) where rnf = rwhnf
