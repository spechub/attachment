import Text.XML.Expat.SAX
import Text.XML.Expat.Format
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = B.getContents >>= B.putStrLn . formatSAX . parse
  (defaultParseOptions :: ParseOptions String String)
