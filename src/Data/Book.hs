{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Book where

import           Control.Applicative
import           Control.DeepSeq
import           Data.Char
import           Data.Fixed
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import           GHC.Generics (Generic)
import qualified SAX
import           Test.QuickCheck
import qualified Text.XML.DOM.Parser as XDP
import qualified Text.XML.Writer as XW
import qualified Xmlbf

data Book = Book
  { author      :: Text
  , title       :: Text
  , genre       :: Text
  , price       :: Centi
  , publishDate :: Day
  , description :: Text
  } deriving (Show, Generic)

instance NFData Book

instance Arbitrary Book where
  arbitrary = Book
    <$> arbText
    <*> arbText
    <*> arbText
    <*> arbPrice
    <*> arbDay
    <*> arbText
    where
      arbText  = T.pack <$> listOf1 (suchThat arbitrary (liftA2 (&&) isAscii isAlphaNum))
      arbPrice = suchThat arbitrary (> 0)
      arbDay   = ModifiedJulianDay <$> choose (1,100000)

instance XW.ToXML Book where
  toXML b = XW.element "book" $ do
    XW.element "author" $ XW.content (author b)
    XW.element "title" $ XW.content (title b)
    XW.element "genre" $ XW.content (genre b)
    XW.element "price" $ XW.content (T.pack . showFixed True $ price b)
    XW.element "publish_date" $ XW.content (T.pack . formatTime defaultTimeLocale "%F" $ publishDate b)
    XW.element "description" $ XW.content (description b)

instance XDP.FromDom Book where
  fromDom = Book
    <$> XDP.inElem "author" XDP.textFromDom
    <*> XDP.inElem "title" XDP.textFromDom
    <*> XDP.inElem "genre" XDP.textFromDom
    <*> XDP.inElem "price" (XDP.parseContent XDP.readContent)
    <*> XDP.inElem "publish_date" (XDP.parseContent (parseTimeM True defaultTimeLocale "%F" . T.unpack))
    <*> XDP.inElem "description" XDP.textFromDom

instance Xmlbf.ToXml Book where
  toXml b =
    [ Xmlbf.element' "book" mempty
      [ Xmlbf.element' "author" mempty
        [ Xmlbf.text $ author b ]
      , Xmlbf.element' "title" mempty
        [ Xmlbf.text $ title b ]
      , Xmlbf.element' "genre" mempty
        [ Xmlbf.text $ genre b ]
      , Xmlbf.element' "price" mempty
        [ Xmlbf.text . T.pack . showFixed True $ price b ]
      , Xmlbf.element' "publish_date" mempty
        [ Xmlbf.text . T.pack . formatTime defaultTimeLocale "%F" $ publishDate b ]
      , Xmlbf.element' "description" mempty
        [ Xmlbf.text $ description b ]
      ]
    ]

instance Xmlbf.FromXml Book where
  fromXml = Xmlbf.pElement "book" $ Book
    <$> Xmlbf.pElement "author" Xmlbf.pText
    <*> Xmlbf.pElement "title" Xmlbf.pText
    <*> Xmlbf.pElement "genre" Xmlbf.pText
    <*> Xmlbf.pElement "price" (Xmlbf.pText >>= Xmlbf.pRead)
    <*> Xmlbf.pElement "publish_date" (Xmlbf.pText >>= parseTimeM True defaultTimeLocale "%F" . T.unpack)
    <*> Xmlbf.pElement "description" Xmlbf.pText

bookSaxParser :: SAX.SaxParser Book
bookSaxParser = SAX.atTag "book" $ Book
  <$> SAX.atTag "author" saxText
  <*> SAX.atTag "title" saxText
  <*> SAX.atTag "genre" saxText
  <*> SAX.atTag "price" (read . T.unpack <$> saxText)
  <*> SAX.atTag "publish_date" (saxText >>= parseTimeM True defaultTimeLocale "%F" . T.unpack)
  <*> SAX.atTag "description" saxText
  where
    saxText = T.decodeUtf8 <$> SAX.bytes

newtype Catalog = Catalog [Book]
  deriving (Show, Generic, NFData)

instance Arbitrary Catalog where
  arbitrary = Catalog <$> listOf arbitrary

instance XW.ToXML Catalog where
  toXML (Catalog books) = XW.element "catalog" $ mapM_ XW.toXML books

instance XDP.FromDom Catalog where
  fromDom = Catalog <$> XDP.inElemAll "book" XDP.fromDom

instance Xmlbf.FromXml Catalog where
  fromXml = Xmlbf.pElement "catalog" $ Catalog
    <$> many Xmlbf.fromXml

catalogSaxParser :: SAX.SaxParser Catalog
catalogSaxParser = SAX.atTag "catalog" $ Catalog
  <$> many bookSaxParser

newtype Root = Root Catalog
  deriving (Show, Generic, NFData)

instance Xmlbf.FromXml Root where
  fromXml = Xmlbf.pElement "root" $ Root
    <$> Xmlbf.fromXml

instance XDP.FromDom Root where
  fromDom = Root <$> XDP.inElem "root" XDP.fromDom

rootSaxParser :: SAX.SaxParser Root
rootSaxParser = SAX.atTag "root" $ Root
  <$> catalogSaxParser
