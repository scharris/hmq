{-# OPTIONS_GHC -fbang-patterns #-}

module HMQ.Example.GeneratedEntities.Testtableownedbypg where

import Data.List
import Data.Maybe
import System.Time
import Database.HDBC

import HMQ.Metadata.TableMetadata
import HMQ.Query hiding(tableIdentifier)
import HMQ.ForeignKeyLink
import HMQ.MappedQuery
import HMQ.RowValueExtractors



data Testtableownedbypg = 
    Testtableownedbypg
     {
          field1 :: Integer,
          field2 :: String
     }
     deriving (Show)



-- The extractor function which creates a Testtableownedbypg from a result row.
rowValueExtractor ::  EntityRowValueExtractor Testtableownedbypg
rowValueExtractor !tableAlias !qry =
    let
        leadingColIx = fromMaybe (error $ "No columns for table alias " ++ tableAlias ++ " found in query.") $
                                 findIndex ((tableAlias ++ ".") `isPrefixOf`) (selectFieldExprs qry)
    in
      leadingColIx `seq`
          (\(!row) ->
              let
                  fv1:fv2:_ = drop leadingColIx row
              in
                -- Check first pk field to determine if there's a value for this row
                if fv1 == SqlNull then
                    Nothing
                else
                    Just $ Testtableownedbypg (fromSql fv1) (fromSql fv2)
          )


tableMetadata :: TableMetadata
tableMetadata = 
    TableMetadata {
     tableIdentifier = TableIdentifier {tableSchema = Just "public", tableName = "testtableownedbypg"},
     fieldMetadatas =
       [
           FieldMetadata {fieldName = "field1", fieldSqlColDesc = SqlColDesc {colType = SqlBigIntT, colSize = Just 4, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "field2", fieldSqlColDesc = SqlColDesc {colType = SqlVarCharT, colSize = Just 200, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}}
       ],
     primaryKeyFieldNames = ["field1"],
     foreignKeyConstraints = []
}


mappedQuery :: MappedQuery Testtableownedbypg Testtableownedbypg Testtableownedbypg
mappedQuery = MappedTable tableMetadata rowValueExtractor

