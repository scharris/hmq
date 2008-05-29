{-# OPTIONS_GHC -fbang-patterns #-}

module HMQ.Example.GeneratedEntities.Rentals where

import Data.List
import Data.Maybe
import System.Time
import Database.HDBC

import HMQ.Metadata.TableMetadata
import HMQ.Query hiding(tableIdentifier)
import HMQ.ForeignKeyLink
import HMQ.MappedQuery
import HMQ.RowValueExtractors



data Rental = 
    Rental
     {
          rentalId :: Integer,
          discId :: String,
          rentalDate :: CalendarTime,
          returnDate :: CalendarTime,
          customerId :: Integer,
          employeeId :: Integer,
          storeId :: Integer
     }
     deriving (Show)



-- The extractor function which creates a Rental from a result row.
rowValueExtractor ::  EntityRowValueExtractor Rental
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
                    Just $ Rental (fromSql fv1) (fromSql fv2) (fromSql fv3) (fromSql fv4) (fromSql fv5) (fromSql fv6) (fromSql fv7)
          )


tableMetadata :: TableMetadata
tableMetadata = 
    TableMetadata {
     tableIdentifier = TableIdentifier {tableSchema = Just "public", tableName = "rentals"},
     fieldMetadatas =
       [
           FieldMetadata {fieldName = "rental_id", fieldSqlColDesc = SqlColDesc {colType = SqlBigIntT, colSize = Just 4, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "disc_id", fieldSqlColDesc = SqlColDesc {colType = SqlCharT, colSize = Just 8, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "rental_date", fieldSqlColDesc = SqlColDesc {colType = SqlDateT, colSize = Just 4, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "return_date", fieldSqlColDesc = SqlColDesc {colType = SqlDateT, colSize = Just 4, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "customer_id", fieldSqlColDesc = SqlColDesc {colType = SqlBigIntT, colSize = Just 4, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "employee_id", fieldSqlColDesc = SqlColDesc {colType = SqlBigIntT, colSize = Just 4, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "store_id", fieldSqlColDesc = SqlColDesc {colType = SqlBigIntT, colSize = Just 4, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}}
       ],
     primaryKeyFieldNames = ["rental_id"],
     foreignKeyConstraints = [ForeignKeyConstraint {sourceTable = TableIdentifier {tableSchema = Just "public", tableName = "rentals"}, targetTable = TableIdentifier {tableSchema = Just "public", tableName = "discs"}, equatedFields = [("disc_id","disc_id")]},ForeignKeyConstraint {sourceTable = TableIdentifier {tableSchema = Just "public", tableName = "rentals"}, targetTable = TableIdentifier {tableSchema = Just "public", tableName = "customers"}, equatedFields = [("customer_id","customer_id")]},ForeignKeyConstraint {sourceTable = TableIdentifier {tableSchema = Just "public", tableName = "rentals"}, targetTable = TableIdentifier {tableSchema = Just "public", tableName = "employees"}, equatedFields = [("employee_id","employee_id")]},ForeignKeyConstraint {sourceTable = TableIdentifier {tableSchema = Just "public", tableName = "rentals"}, targetTable = TableIdentifier {tableSchema = Just "public", tableName = "stores"}, equatedFields = [("store_id","store_id")]}]
}


mappedQuery :: MappedQuery Rental Rental Rental
mappedQuery = MappedTable tableMetadata rowValueExtractor

