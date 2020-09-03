{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O -ddump-splices -ddump-simpl -dsuppress-all -ddump-to-file #-}
module Main where

import qualified Data.ByteString.Lazy               as L
import           Data.ByteString.Builder
import           Data.Monoid
import           Data.Foldable                        (foldMap)
import           Data.List                            (intersperse)

import Criterion.Main
import Data.Int (Int64)

import Data.Aeson.TH
import Data.Aeson (ToJSON (..), fromEncoding)

data BinTree = TreeLeaf | TreeNode { leftTree :: BinTree, rightTree :: BinTree }

$(deriveToJSON defaultOptions ''BinTree)

mkTree :: Int -> BinTree
mkTree 0 = TreeLeaf
mkTree n = let t = mkTree (n-1) in TreeNode t t

doit :: Int -> Int64
doit n = L.length $ toLazyByteString $ fromEncoding $ toEncoding $ mkTree n

main :: IO ()
main = defaultMain
  [
    bgroup "Labels"
      [
        bench "BinTree" $ whnf doit 22
      ]
  ]


