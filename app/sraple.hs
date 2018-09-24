{-# LANGUAGE OverloadedStrings #-}

import Data.Tree
import Text.HTML.Scalpel
import Control.Applicative
import Control.Monad
import Data.List (isInfixOf)
import Network.Curl hiding (content)

url = "https://seekingalpha.com/article/4132283-jabils-jbl-ceo-mark-mondello-q1-2018-results-earnings-call-transcript?all=true&find=conference%2520call"

opts = [ CurlUserAgent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_8) AppleWebKit/534.30 (KHTML, like Gecko) Chrome/12.0.742.112 Safari/534.30" ]

e content = scrapeURL "https://www.baidu.com" content

m content = scrapeURL "https://www.linkedin.com/in/chenyu-xiong-0abb6a91/" content

d content= scrapeURL "https://seekingalpha.com/article/4132283-jabils-jbl-ceo-mark-mondello-q1-2018-results-earnings-call-transcript?all=true&find=conference%2520call" content

exampleHtml :: String
exampleHtml = "<html>\
\    <body>\
\        <div class='comments'>\
\            <div class='comment container'>\
\                <span class='comment author'>Sally</span>\
\                <div class='comment text'>Woo hoo!</div>\
\            </div>\
\            <div class='comment container'>\
\                <span class='comment author'>Bill</span>\
\                <img class='comment image' src='http://example.com/cat.gif' />\
\            </div>\
\            <div class='comment container'>\
\                <span class='comment author'>Bertrand</span>\
\                <div class='comment text'>That sure is some cat!</div>\
\            </div>\
\            <div class='comment container'>\
\                <span class='comment author'>Susan</span>\
\                <div class='comment text'>WTF!?!</div>\
\            </div>\
\        </div>\
\    </body>\
\</html>"

main :: IO ()
main = do
    html <- scrapeURLWithOpts opts url $ htmls anySelector
    maybe printError printHtml html
  where
    printError = putStrLn "Failed"
    printHtml = mapM_ putStrLn

catComment :: Scraper String String
catComment =
    -- 1. First narrow the current context to the div containing the comment's
    --    textual content.
    chroot ("div" @: [hasClass "comment", hasClass "text"]) $ do
        -- 2. Any can be used to access the root tag of the current context.
        contents <- text anySelector
        -- 3. Skip comment divs that do not contain "cat".
        guard ("cat" `isInfixOf` contents)
        -- 4. Generate the desired value.
        html anySelector

data Category
    = Title String
    | Contents String
    deriving (Show, Eq)

sc:: Scraper String [Category]
sc = chroots ("p" @: [hasClass "p1"]) $ do
        contents <- title <|> content
        return $ contents
        where strongP = 1

title = do
  t <- text $ ("strong")
  return $ Title t

content = do
  t <- text $ anySelector
  return $ Contents t

categorise :: Category -> [Category] -> [[Category]] -> [[Category]]
categorise (Title q) [] a= a ++ [[Title q]]

categorise (Contents q) [] a= reverse([((head $ reverse a) ++[Contents q])] ++ (tail $ reverse a))
categorise w (x:xs) a = categorise x xs (categorise w [] a)

fd =  do
 aaa <- d sc
 case aaa of
   Nothing -> putStrLn "No data"
   Just cat -> print (categorise (head cat) (tail cat) [[]])

dataTree :: Tree Category
dataTree = Node (Title "Body") []

a :: [Maybe Integer]
a = [Nothing,Nothing, Just 1 ,Just 6, Nothing, Just 3]

b :: [Tree (Maybe Integer)]
b = [Node Nothing [Node Nothing [Node (Just 1) [], Node (Just 6) []]], Node Nothing [Node (Just 3) []]]

lastElement :: Tree Category -> Category
lastElement = last . flatten

-- treeRise :: [Category] -> Category -> Tree Category -> Tree Category
-- treeRise [] lastValue tree = tree
-- treeRise [(Title x)] lastValue tree = case lastValue of
--   -- Title t ->
--   -- Contents c ->
-- treeRise [(Contents x)] lastValue tree = undefined
-- treeRise (x:xs) lastValue tree = treeRise xs (lastElement newTree) newTree
--             where newTree = treeRise [x] (lastElement tree) tree
