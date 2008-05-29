{-# OPTIONS_GHC -fbang-patterns #-}

module HMQ.Example.GeneratedEntities.Categories where

import Data.List
import Data.Maybe
import System.Time
import Database.HDBC

import HMQ.Metadata.TableMetadata
import HMQ.Query hiding(tableIdentifier)
import HMQ.ForeignKeyLink
import HMQ.MappedQuery
import HMQ.RowValueExtractors



data Category = 
    Category
     {
          categoryId :: Integer,
          name :: String
     }
     deriving (Show)



-- The extractor function which creates a Category from a result row.
rowValueExtractor ::  EntityRowValueExtractor Category
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
                    Just $ Category (fromSql fv1) (fromSql fv2)
          )


tableMetadata :: TableMetadata
tableMetadata = 
    TableMetadata {
     tableIdentifier = TableIdentifier {tableSchema = Just "public", tableName = "categories"},
     fieldMetadatas =
       [
           FieldMetadata {fieldName = "category_id", fieldSqlColDesc = SqlColDesc {colType = SqlBigIntT, colSize = Just 4, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "name", fieldSqlColDesc = SqlColDesc {colType = SqlVarCharT, colSize = Just 80, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}}
       ],
     primaryKeyFieldNames = ["category_id"],
     foreignKeyConstraints = []
}


mappedQuery :: MappedQuery Category Category Category
mappedQuery = MappedTable tableMetadata rowValueExtractor

