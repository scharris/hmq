-- Copyright (c) 2008 Stephen C. Harris.
-- See COPYING file at the root of this distribution for copyright information.

module HMQ.Utils.Strings where

import Data.Char
import Data.List
import Data.Maybe
import Text.Regex



camelCase :: Bool -> String -> String
camelCase initialCapital s =
    let cc = concatMap (capitalize . map toLower) $ split '_' s
    in if initialCapital then cc else unCapitalize cc


capitalize :: String -> String
capitalize "" = ""
capitalize (c:cs) = toUpper c : cs
    

lowerCase :: String -> String
lowerCase = map toLower

upperCase :: String -> String
upperCase = map toUpper

unCapitalize :: String -> String
unCapitalize "" = ""
unCapitalize (c:cs) = toLower c : cs


split :: Char -> String -> [String]
split = unfoldr . split'

split' :: Char -> String -> Maybe (String, String)
split' c [] = Nothing
split' c l = Just (h, drop 1 t)
  where (h, t) = span (/=c) l


unQualifyEmbeddedNames :: String -> String
unQualifyEmbeddedNames s = subRegex (mkRegex "([A-Za-z_][A-Za-z_0-9]*\\.)+([A-Za-z_][A-Za-z_0-9]*)") s "\\2"


unqualify :: String -> String;
unqualify = unQualifyEmbeddedNames


contains :: String -> String -> Bool
contains = isInfixOf


indent :: Int -> String -> String
indent n s = 
    let nSpaces = replicate n ' '
    in listAsString (\l -> nSpaces ++ l) "\n" (lines s)


listAsString :: (a -> String) -> String -> [a] -> String
listAsString f sep list = concat $ intersperse sep $ map f list


stringListLiteral :: [String] -> String
stringListLiteral ss = "[" ++ listAsString quote ", " ss ++ "]"


quote :: String -> String
quote s = "\"" ++ escapeDoubleQuotes s ++ "\""

escapeDoubleQuotes :: String -> String
escapeDoubleQuotes s = subRegex (mkRegex "\"") s "\\\""

escapeBackslashes :: String -> String
escapeBackslashes s = subRegex (mkRegex "\\\\") s "\\\\\\\\"

escapeNewlines :: String -> String
escapeNewlines s = subRegex (mkRegex "\n") s "\\\\n"


indefiniteArticleFor :: String -> String;
indefiniteArticleFor "" = ""
indefiniteArticleFor (c:_) = 
    if elem (toLower c) ['a','e','i','o','u'] then
        "an"
    else
        "a"

-- TODO: need a much richer set of patterns here
singularize :: String -> String
singularize name = 
    let  singularizationPatterns = [("(.*)ses$", "\\1sis"),
                                    ("(.*)ies$", "\\1y"),
                                    ("(.*)s$",   "\\1")];
    in
        case find (\(patStr,_) -> isJust $ matchRegex (mkRegexWithOpts patStr True True) name)
                  singularizationPatterns of                                       
          
          Just (patStr,repl) -> 
              let 
                  pat = mkRegexWithOpts patStr True True -- case insensitive
              in
                subRegex pat name repl
          
          Nothing -> name
       
