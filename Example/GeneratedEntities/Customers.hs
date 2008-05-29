{-# OPTIONS_GHC -fbang-patterns #-}

module HMQ.Example.GeneratedEntities.Customers where

import Data.List
import Data.Maybe
import System.Time
import Database.HDBC

import HMQ.Metadata.TableMetadata
import HMQ.Query hiding(tableIdentifier)
import HMQ.ForeignKeyLink
import HMQ.MappedQuery
import HMQ.RowValueExtractors



data Customer = 
    Customer
     {
          customerId :: Integer,
          lastName :: String,
          firstName :: String,
          phone :: String,
          birthDate :: CalendarTime,
          accountId :: Integer,
          spendinglimit :: Rational
     }
     deriving (Show)



-- The extractor function which creates a Customer from a result row.
rowValueExtractor ::  EntityRowValueExtractor Customer
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
                    Just $ Customer (fromSql fv1) (fromSql fv2) (fromSql fv3) (fromSql fv4) (fromSql fv5) (fromSql fv6) (fromSql fv7)
          )


tableMetadata :: TableMetadata
tableMetadata = 
    TableMetadata {
     tableIdentifier = TableIdentifier {tableSchema = Just "public", tableName = "customers"},
     fieldMetadatas =
       [
           FieldMetadata {fieldName = "customer_id", fieldSqlColDesc = SqlColDesc {colType = SqlBigIntT, colSize = Just 4, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "last_name", fieldSqlColDesc = SqlColDesc {colType = SqlVarCharT, colSize = Just 50, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "first_name", fieldSqlColDesc = SqlColDesc {colType = SqlVarCharT, colSize = Just 50, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "phone", fieldSqlColDesc = SqlColDesc {colType = SqlVarCharT, colSize = Just 12, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "birth_date", fieldSqlColDesc = SqlColDesc {colType = SqlDateT, colSize = Just 4, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "account_id", fieldSqlColDesc = SqlColDesc {colType = SqlBigIntT, colSize = Just 4, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "spendinglimit", fieldSqlColDesc = SqlColDesc {colType = SqlNumericT, colSize = Just 7, colOctetLength = Nothing, colDecDigits = Just 2, colNullable = Just False}}
       ],
     primaryKeyFieldNames = ["customer_id"],
     foreignKeyConstraints = [ForeignKeyConstraint {sourceTable = TableIdentifier {tableSchema = Just "public", tableName = "customers"}, targetTable = TableIdentifier {tableSchema = Just "public", tableName = "accounts"}, equatedFields = [("account_id","account_id")]}]
}


mappedQuery :: MappedQuery Customer Customer Customer
mappedQuery = MappedTable tableMetadata rowValueExtractor

