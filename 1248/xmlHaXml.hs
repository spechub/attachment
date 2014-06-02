import Text.XML.HaXml
import Text.XML.HaXml.Pretty

main :: IO ()
main = getContents >>= putStrLn . render . document . xmlParse "stdin"
