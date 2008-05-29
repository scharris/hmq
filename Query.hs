module HMQ.Query where

-- Copyright (c) 2008 Stephen C. Harris.
-- See COPYING file at the root of this distribution for copyright information.

import Data.Maybe


import HMQ.Metadata.TableMetadata
import HMQ.Utils.Strings




data Query =
	Query
        {
            selectFields :: [(String, Maybe String)],
	    fromClause :: String,
	    whereConditions :: [String]
        }
    deriving (Show)

selectFieldExprs :: Query -> [String]
selectFieldExprs = map fst . selectFields

selectFieldNames :: Query -> [Maybe String]
selectFieldNames = map snd . selectFields


toSqlString :: Query -> String
toSqlString (Query selFlds fromClause restrs) = 
    let
	selectFieldString (expr,maybeAlias) = expr ++ maybe "" (\alias -> " AS \"" ++ alias ++ "\"") maybeAlias
    in
      "SELECT " ++ listAsString selectFieldString "," selFlds ++ "\n" ++
      "FROM " ++ fromClause ++ "\n" ++
      if null restrs then ""
       else "WHERE\n" ++ 
           foldl (\accumRestrs r -> (if accumRestrs == "" then "  " else accumRestrs ++ "\n  AND ") ++ "(" ++ r ++ ")")
                 ""
                 restrs

data QueryTable = 
    QueryTable 
    {
      tableIdentifier :: TableIdentifier,
      tableAlias ::  TableAlias
    }
    deriving(Show)

type TableAlias = String




