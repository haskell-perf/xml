{-# LANGUAGE OverloadedStrings #-}

import           Data.Book
import           Data.Default
import           Test.QuickCheck
import qualified Text.XML
import qualified Text.XML.Writer

main :: IO ()
main = do
  let fp = "in.xml"
  catalog <- Catalog <$> generate (vector 5000)
  let doc = Text.XML.Writer.document "root" (Text.XML.Writer.toXML catalog)
  Text.XML.writeFile def fp doc
