{-# OPTIONS_GHC -fbang-patterns #-}

module HMQ.Example.GeneratedEntities.Stores where

import Data.List
import Data.Maybe
import System.Time
import Database.HDBC

import HMQ.Metadata.TableMetadata
import HMQ.Query hiding(tableIdentifier)
import HMQ.ForeignKeyLink
import HMQ.MappedQuery
import HMQ.RowValueExtractors



data Store = 
    Store
     {
          storeId :: Integer,
          street :: String,
          city :: String,
          state :: String,
          zip :: String,
          openTime :: String,
          closeTime :: String
     }
     deriving (Show)



-- The extractor function which creates a Store from a result row.
rowValueExtractor ::  EntityRowValueExtractor Store
rowValueExtractor !tableAlias !qry =
    let
        leadingColIx = fromMaybe (error $ "No columns for table alias " ++ tableAlias ++ " found in query.") $
                                 findIndex ((tableAlias ++ ".") `isPrefixOf`) (selectFieldExprs qry)
    in
      leadingColIx `seq`
          (\(!row) ->
              let
                  fv1:fv2:fv3:fv4:fv5:fv6:fv7:_ = drop leadingColIx row
              in
                -- Check first pk field to determine if there's a value for this row
                if fv1 == SqlNull then
                    Nothing
                else
                    Just $ Store (fromSql fv1) (fromSql fv2) (fromSql fv3) (fromSql fv4) (fromSql fv5) (fromSql fv6) (fromSql fv7)
          )


tableMetadata :: TableMetadata
tableMetadata = 
    TableMetadata {
     tableIdentifier = TableIdentifier {tableSchema = Just "public", tableName = "stores"},
     fieldMetadatas =
       [
           FieldMetadata {fieldName = "store_id", fieldSqlColDesc = SqlColDesc {colType = SqlBigIntT, colSize = Just 4, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "street", fieldSqlColDesc = SqlColDesc {colType = SqlVarCharT, colSize = Just 50, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "city", fieldSqlColDesc = SqlColDesc {colType = SqlVarCharT, colSize = Just 50, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "state", fieldSqlColDesc = SqlColDesc {colType = SqlCharT, colSize = Just 2, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "zip", fieldSqlColDesc = SqlColDesc {colType = SqlVarCharT, colSize = Just 10, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "open_time", fieldSqlColDesc = SqlColDesc {colType = SqlVarCharT, colSize = Just 8, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "close_time", fieldSqlColDesc = SqlColDesc {colType = SqlVarCharT, colSize = Just 8, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}}
       ],
     primaryKeyFieldNames = ["store_id"],
     foreignKeyConstraints = []
}


mappedQuery :: MappedQuery Store Store Store
mappedQuery = MappedTable tableMetadata rowValueExtractor

