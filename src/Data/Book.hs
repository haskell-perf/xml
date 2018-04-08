{-# LANGUAGE OverloadedStrings #-}

module Data.Book where

import           Data.Char
import           Data.Fixed
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Test.QuickCheck
import qualified Xmlbf
import qualified Text.XML.Writer as XW

data Book = Book
  { author      :: Text
  , title       :: Text
  , genre       :: Text
  , price       :: Centi
  , publishDate :: Day
  , description :: Text
  }

instance Arbitrary Book where
  arbitrary = Book
    <$> arbText
    <*> arbText
    <*> arbText
    <*> arbPrice
    <*> arbDay
    <*> arbText
    where
      arbText  = T.pack <$> listOf (suchThat arbitrary isPrint)
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

newtype Catalog = Catalog [Book]

instance Arbitrary Catalog where
  arbitrary = Catalog <$> listOf arbitrary

instance XW.ToXML Catalog where
  toXML (Catalog books) = XW.element "catalog" $ mapM_ XW.toXML books
