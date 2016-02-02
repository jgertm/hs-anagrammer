module Main where

import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString       as ByteString8
import qualified Data.HashMap.Strict   as Map
import qualified Data.IntMap           as IntMap
import qualified Data.List             as List
import qualified Data.Set              as Set
import Data.Maybe (fromMaybe)
import Data.Foldable (foldl')
import Data.ByteString.Char8 (ByteString)
import Data.Monoid ((<>))
import Control.Monad (forM_)
import System.Environment (getArgs)

main :: IO ()
main = do
    -- read the filename:
    args <- getArgs

    -- load the file:
    file <- ByteString.readFile (if null args then "shakespeare.txt" else head args)

    -- parse the file, find anagrams and output stats:
    outputStats (anagrams $ ByteString.splitWith (== '\n') file)

-- tokenise a line into words (all lowercase)
tokenise :: ByteString -> [ByteString]
tokenise = filter (/= "") . ByteString.splitWith notAlpha . ByteString8.map lowerCase
  where
    notAlpha  c = c < 'A' || c > 'z' || (c > 'Z' && c < 'a')
    lowerCase c = if c > 64 && c < 91 then c+32 else c

-- | This hash does not change under permutations and adding nonAlpha symbols.
hash :: ByteString -> Int
hash = ByteString8.foldl' f 0
  where
    f acc c
      | c > 64 && c < 91  = acc + (fromIntegral c - 64) ^ (4 :: Int)
      | c > 96 && c < 123 = acc + (fromIntegral c - 96) ^ (4 :: Int)
      | otherwise         = acc

anagrams :: [ByteString] -> [[ByteString]]
anagrams = foldMap (anagrams' . Set.toList)
         . IntMap.filter (\s -> Set.size s > 1)
         . foldl' (\acc xs -> IntMap.insertWith (<>) (hash xs) (Set.singleton xs) acc) mempty
         . filter (not . ByteString8.null)

-- take in lines of words, and output groups of anagram lines:
anagrams' :: [ByteString] -> [[ByteString]]
anagrams' = foldl' collapse [] . foldl' mappify Map.empty
  where
    -- step 1: collect into a map:
    mappify !m ""   = m
    mappify !m orig =
        let !toks = tokenise orig
            !wkey = List.sort toks
            !key  = ByteString.sort $ ByteString.concat toks
            !cur  = fromMaybe [] (Map.lookup key m)
        in  Map.insert key ((wkey,orig) : cur) m

    -- step 2: reduce into groups:
    collapse list ws = case dedupe ws of
        deduped@(_ : _ : _) -> deduped : list
        _ -> list

-- output stats:
outputStats :: [[ByteString]] -> IO ()
outputStats matches = do
    forM_ (List.sort matches) $ \ms -> do
        forM_ ms ByteString.putStrLn
        ByteString.putStr "\n"
    putStrLn $ (show $ length matches) ++ " anagram sets found."

-- take key-value pairs and return values, deduping by keys
dedupe :: Ord key => [(key,val)] -> [val]
dedupe lark = run lark Set.empty [] where
    run [] _ !out = out
    run ((key,val):as) !s !out = if Set.member key s
        then run as s out
        else run as (Set.insert key s) (val:out)
