module Main where

import SimpleJSON

main = print (JObject [("foo", JNumber 1), ("bar", JString "str")])
