import Text.XML.Expat.Tree
import Text.XML.Expat.Format
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = B.getContents >>= B.putStrLn . format . fst . parse
  (defaultParseOptions :: ParseOptions String String)
