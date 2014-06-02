import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree

main :: IO ()
main = getContents >>= putStr . renderTags . flattenTree . tagTree . parseTags
