module SimpleJSON
    (JValue(..)
    , getString
    , getDouble
    , getInt
    , getBool
    , isNull
    , getArray
    , getObject
    ) where

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String 
getString (JString val) = Just val
getString _             = Nothing

getInt :: JValue -> Maybe Int 
getInt (JNumber val) = Just . truncate $ val
getInt _             = Nothing

getDouble :: JValue -> Maybe Double 
getDouble (JNumber val) = Just val
getDouble _             = Nothing

getBool :: JValue -> Maybe Bool 
getBool (JBool val) = Just val
getBool _           = Nothing

isNull :: JValue -> Bool 
isNull JNull = True
isNull _     = False

getArray :: JValue -> Maybe [JValue] 
getArray (JArray val) = Just val
getArray _            = Nothing

getObject :: JValue -> Maybe [(String, JValue)] 
getObject (JObject val) = Just val
getObject _             = Nothing

