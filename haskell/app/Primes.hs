{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           ClassyPrelude   hiding (filter, (<>))
import           Data.List       (nubBy)
import           Data.Monoid
import           Data.Witherable


type Rep = String

charToPrime :: Map Char Integer
charToPrime = mapFromList $ zip chars primes
  where chars = ['e','t','o','a','i','s','n','h','r','l','d','u','m','y','w','c','f','g','b','p','v','k','x','j','q','z']
        primes = 2 : [n | n <- [3..], foldr (\p r-> p*p > n || rem n p > 0 && r) True primes]

clean :: Rep -> [Rep]
clean =  filter multiple . lines

tokenise :: Rep -> Rep
tokenise = toLower

productise :: Rep -> Integer
productise = getProduct . concatMap (Product . fromMaybe 1 . flip lookup charToPrime)

anagrams :: Rep -> [[Rep]]
anagrams = filter multiple
         . fmap (nubBy (\a b -> op a == op b) . toList)
         . toList
         . (id :: Map Integer (Set Rep) -> Map Integer (Set Rep))
         . foldr (\(prod, strL) acc -> insertWith (<>) prod strL acc) mempty
         . fmap (productise . tokenise &&& singletonSet)
         . clean

op :: Rep -> [Rep]
op = sort . words . filter (\c -> c > 'a' && c < 'z') . toLower

multiple :: MonoFoldable mono => mono -> Bool
multiple s = (==) GT $ compareLength s (1::Int)

main :: IO ()
main = do
  args <- getArgs
  let path = repack <$> fromMaybe "shakespeare.txt" $ headMay args
  file <- repack <$> readFileUtf8 path
  let matches = anagrams file

  forM_ matches $ \ms -> do
      forM_ ms sayString
      say "\n"
  sayString $ (show $ length matches) ++ " anagram sets found."
