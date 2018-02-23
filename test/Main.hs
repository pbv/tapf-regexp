{-|
  Examples of regular expression matching
-}
{-# LANGUAGE ParallelListComp #-}
module Main where

import TAPF.Regexp

regexps :: [Regexp]
regexps
  = [ many (string "a") <+> many (string "b")
    , many (string "a" <+> string "b")
    , many (string "a") <> many (string "b")
    , many (string "b") <> many (string "a")
    , many (string "ab")
    ]

strings :: Int -> [String]
strings len
  = [ replicate len 'a' ++ replicate len 'b'
    , replicate len 'b' ++ replicate len 'a'
    , concat (replicate len "ab")
    ]

main :: IO ()
main = 
  sequence_ [ do { putStrLn ("Example " ++ show n)
                 ; print re
                 ; print str
                 ; print (match re str)
                 ; putStrLn ""
                 }
            | re <- regexps, str <- strings 20
            | n <- [1..]
            ]
