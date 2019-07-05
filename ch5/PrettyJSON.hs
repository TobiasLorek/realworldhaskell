module PrettyJSON where

import Data.List (intercalate)
import SimpleJSON
import Prettify
    ( Doc
    , (<>)
    , char
    , double
    , text
    , string
    , series
    , fsep
    , hcat
    , punctuate
    , compact
    )


renderJValue :: JValue -> Doc
renderJValue (JString s)   = string s
renderJValue (JNumber n)   = double n
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
renderJValue (JArray arr)  = series '[' ']' renderJValue arr
renderJValue (JObject o)   = series '{' '}' field o
    where field (name, val)= string name
                          <> text ": "
                          <> renderJValue val


