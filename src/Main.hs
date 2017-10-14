module Main where

import AST(MessageSpec)
import qualified Parser.Atto as Atto
import qualified Parser.Earley as Earley
import qualified Parser.Mega as Mega
import qualified Parser.Happy as Happy

import Criterion.Main
import Data.Text(Text)
import qualified Data.Text.IO as T

main :: IO ()
main = defaultMain
    [ bgroup "attoparsec" $ benches Atto.parseMessage
    , bgroup "earley" $ benches Earley.parseMessage
    , bgroup "megaparsec" $ benches Mega.parseMessage
    , bgroup "alex+happy" $ benches Happy.parseMessage
    ]

benches :: (Text -> Either String MessageSpec) -> [Benchmark]
benches testF =
    [ env (T.readFile "test1.proto") $ 
        \file -> bench "test 1" $ nf testF file
    , env (T.readFile "test2.proto") $ 
        \file -> bench "test 2" $ nf testF file
    , env (T.readFile "test3.proto") $ 
        \file -> bench "test 3" $ nf testF file
    ]