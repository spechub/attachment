import Text.XML.Light
import Data.Maybe
import qualified Data.Text.Lazy.IO as B

main :: IO ()
main = B.getContents >>= putStrLn . ppTopElement . fromJust . parseXMLDoc
