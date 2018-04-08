{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Control.DeepSeq
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy
import           Data.Default
import           Data.FileEmbed
import qualified Foreign.ForeignPtr
import qualified Text.XML
import qualified Text.XML.Expat.Tree
import qualified Text.XML.Hexml
import           Weigh
import qualified Xeno.DOM
#ifdef LIBXML
import qualified Text.XML.LibXML
#endif

main :: IO ()
main = mainWith $ do
  setColumns [Case, Allocated, Max, Live, GCs]
  dom inputBs

dom :: ByteString -> Weigh ()
dom bs = do
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

inputBs :: Data.ByteString.ByteString
inputBs = $(embedFile "in.xml")

-- Assuming that DOM representation is strict (NF=WHNF)
instance NFData Text.XML.Hexml.Node where rnf = rwhnf
instance NFData (Foreign.ForeignPtr.ForeignPtr a) where rnf = rwhnf
#ifdef LIBXML
instance NFData Text.XML.LibXML.Document where rnf = rwhnf
#endif
