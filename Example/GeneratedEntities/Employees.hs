{-# OPTIONS_GHC -fbang-patterns #-}

module HMQ.Example.GeneratedEntities.Employees where

import Data.List
import Data.Maybe
import System.Time
import Database.HDBC

import HMQ.Metadata.TableMetadata
import HMQ.Query hiding(tableIdentifier)
import HMQ.ForeignKeyLink
import HMQ.MappedQuery
import HMQ.RowValueExtractors



data Employee = 
    Employee
     {
          employeeId :: Integer,
          lastName :: String,
          firstName :: String,
          ssn :: String,
          phone :: String,
          birthDate :: CalendarTime,
          managerId :: Integer,
          storeId :: Integer
     }
     deriving (Show)



-- The extractor function which creates an Employee from a result row.
rowValueExtractor ::  EntityRowValueExtractor Employee
rowValueExtractor !tableAlias !qry =
    let
        leadingColIx = fromMaybe (error $ "No columns for table alias " ++ tableAlias ++ " found in query.") $
                                 findIndex ((tableAlias ++ ".") `isPrefixOf`) (selectFieldExprs qry)
    in
      leadingColIx `seq`
          (\(!row) ->
              let
                  fv1:fv2:fv3:fv4:fv5:fv6:fv7:fv8:_ = drop leadingColIx row
              in
                -- Check first pk field to determine if there's a value for this row
                if fv1 == SqlNull then
                    Nothing
                else
                    Just $ Employee (fromSql fv1) (fromSql fv2) (fromSql fv3) (fromSql fv4) (fromSql fv5) (fromSql fv6) (fromSql fv7) (fromSql fv8)
          )


tableMetadata :: TableMetadata
tableMetadata = 
    TableMetadata {
     tableIdentifier = TableIdentifier {tableSchema = Just "public", tableName = "employees"},
     fieldMetadatas =
       [
           FieldMetadata {fieldName = "employee_id", fieldSqlColDesc = SqlColDesc {colType = SqlBigIntT, colSize = Just 4, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "last_name", fieldSqlColDesc = SqlColDesc {colType = SqlVarCharT, colSize = Just 50, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "first_name", fieldSqlColDesc = SqlColDesc {colType = SqlVarCharT, colSize = Just 50, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "ssn", fieldSqlColDesc = SqlColDesc {colType = SqlCharT, colSize = Just 9, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "phone", fieldSqlColDesc = SqlColDesc {colType = SqlVarCharT, colSize = Just 12, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "birth_date", fieldSqlColDesc = SqlColDesc {colType = SqlDateT, colSize = Just 4, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "manager_id", fieldSqlColDesc = SqlColDesc {colType = SqlBigIntT, colSize = Just 4, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "store_id", fieldSqlColDesc = SqlColDesc {colType = SqlBigIntT, colSize = Just 4, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}}
       ],
     primaryKeyFieldNames = ["employee_id"],
     foreignKeyConstraints = [ForeignKeyConstraint {sourceTable = TableIdentifier {tableSchema = Just "public", tableName = "employees"}, targetTable = TableIdentifier {tableSchema = Just "public", tableName = "employees"}, equatedFields = [("manager_id","employee_id")]},ForeignKeyConstraint {sourceTable = TableIdentifier {tableSchema = Just "public", tableName = "employees"}, targetTable = TableIdentifier {tableSchema = Just "public", tableName = "stores"}, equatedFields = [("store_id","store_id")]}]
}


mappedQuery :: MappedQuery Employee Employee Employee
mappedQuery = MappedTable tableMetadata rowValueExtractor

