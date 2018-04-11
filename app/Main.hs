module Main where

-- import System.Environment
import Text.XML.HXT.Core
import Text.XML.HXT.Curl
import Text.XML.HXT.TagSoup
import Text.XML.HXT.XPath
import Data.Char
import Control.Concurrent
import Control.Monad
import Data.Function
import Data.List
import qualified Data.Csv as Csv
import qualified Data.ByteString.Lazy.Char8 as BS

main :: IO ()
main = do
  cps <- (read :: String -> [[(String, Int)]]) <$> readFile "test.txt"
  let cps' = map (maximumBy (compare `on` snd)) cps
  let couples = comb 2 characters
  let one = zipWith (\[x, y] (cp, cnt) -> (x, y, cp, cnt)) couples cps'
  let two = zipWith (\[x, y] (cp, cnt) -> (y, x, cp, cnt)) couples cps'
  BS.writeFile "cp.csv" . Csv.encode $ one ++ two

scrape :: IO ()
scrape = do
  let tagss = map combs $ comb 2 chars
  -- [tag] <- getArgs
  -- let tag = head (head tagss)
  let url = "https://www.pixiv.net/novel/tags.php?tag="
  let xpath = "/html/body/div[1]/div/section/div[1]/text()[1]"
  counted <- forM tagss $ \tags ->
    forM tags $ \tag -> do
      putStrLn "plz wait..."
      threadDelay $ 3 * 1000 * 1000
      x <- getCount <$> withXPath (url ++ tag) xpath
      putStrLn $ tag ++ ": " ++ show x
      return (tag, x)
  writeFile "test.txt" $ show counted

getCount :: [String] -> Int
getCount = read . filter isDigit . head

withXPath :: String -> String -> IO [String]
withXPath url xpath = runX $
  readDocument [ withParseHTML yes
               , withTagSoup
               , withCurl []
               , withWarnings no
               ] url
  >>> getXPathTrees xpath
  >>> getText

comb :: Int -> [a] -> [[a]]
comb 0 _ = [[]]
comb _ [] = []
comb n (x : xs) = map (x:) (comb (n - 1) xs) ++ comb n xs


combs :: [[[a]]] -> [[a]]
combs [x, y] = f x y ++ f y x where
  f x' y' = (++) <$> x' <*> y'

-- [((++) <$> x <*> y) ++ ((++) <$> y <*> x) | [x, y] <- comb 2 chars]

chars :: [[String]]
chars =
  [ ["りみ"]
  , ["さあや"]
  , ["かす"]
  , ["あり"]
  , ["たえ"]
  , ["ひま"]
  , ["つぐ"]
  , ["蘭", "らん"]
  , ["とも"]
  , ["モカ"]
  , ["ちさ"]
  , ["イヴ"]
  , ["あや"]
  , ["麻弥", "まや"]
  , ["ひな"]
  , ["さよ"]
  , ["あこ"]
  , ["ゆき"]
  , ["りん"]
  , ["リサ"]
  , ["はぐ"]
  , ["ミシェ", "みさ"]
  , ["ここ"]
  , ["かお"]
  , ["かの", "かのん"]
  ]

characters :: [String]
characters =
  [ "りみ"
  , "沙綾"
  , "香澄"
  , "有咲"
  , "たえ"
  , "ひまり"
  , "つぐみ"
  , "蘭"
  , "巴"
  , "モカ"
  , "千聖"
  , "イヴ"
  , "彩"
  , "麻弥"
  , "日菜"
  , "紗夜"
  , "あこ"
  , "友希那"
  , "燐子"
  , "リサ"
  , "はぐみ"
  , "美咲"
  , "こころ"
  , "薫"
  , "花音"
  ]
