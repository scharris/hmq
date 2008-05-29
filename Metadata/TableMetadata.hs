-- Copyright (c) 2008 Stephen C. Harris.
-- See COPYING file at the root of this distribution for copyright information.

module HMQ.Metadata.TableMetadata where

import Text.Regex.Posix
import Data.Maybe
import Control.Monad
import Control.Exception
import System.Time

import Database.HDBC

import HMQ.Utils.Strings




data TableIdentifier =
    TableIdentifier
    {
      tableSchema :: Maybe String,
      tableName :: String
    }
    deriving (Eq, Ord, Show)


tableIdentifierString :: TableIdentifier -> String
tableIdentifierString (TableIdentifier maybeSchema name) =
    maybe "" (++".") maybeSchema ++ name




data TableMetadata =
    TableMetadata
    {
      tableIdentifier :: TableIdentifier, 
      fieldMetadatas :: [FieldMetadata],
      primaryKeyFieldNames :: [FieldName],
      foreignKeyConstraints :: [ForeignKeyConstraint]
    }

instance Show TableMetadata where
    show = showTableMetadata   
         

showTableMetadata :: TableMetadata -> String
showTableMetadata tmd =
    "TableMetadata {\n"
    ++ "     tableIdentifier = " ++ show (tableIdentifier tmd) ++ ",\n"
    ++ "     fieldMetadatas =\n"
    ++ "       [\n" 
    ++ listAsString (\fieldMd -> "           " ++ show fieldMd) ",\n" (fieldMetadatas tmd) ++ "\n"
    ++ "       ],\n"
    ++ "     primaryKeyFieldNames = " ++ show (primaryKeyFieldNames tmd) ++ ",\n"
    ++ "     foreignKeyConstraints = " ++ show (foreignKeyConstraints tmd) ++ "\n"
    ++ "}"


fieldNames :: TableMetadata -> [String]
fieldNames = map fieldName . fieldMetadatas


data FieldMetadata =
    FieldMetadata
    {
      fieldName :: FieldName,
      fieldSqlColDesc :: SqlColDesc
    }
    deriving (Show)


fieldSqlTypeId :: FieldMetadata -> SqlTypeId
fieldSqlTypeId = colType . fieldSqlColDesc

fieldIsNullable :: FieldMetadata -> Maybe Bool
fieldIsNullable = colNullable . fieldSqlColDesc

fieldKnownNotNullable :: FieldMetadata -> Bool
fieldKnownNotNullable fmd = 
    case fieldIsNullable fmd of
      Just True -> True
      _ -> False

fieldSize :: FieldMetadata -> Maybe Int
fieldSize = colSize . fieldSqlColDesc

fieldScale :: FieldMetadata -> Maybe Int
fieldScale = colDecDigits . fieldSqlColDesc


isSmallIntegral :: FieldMetadata -> Bool
isSmallIntegral fmd = 
    case fieldSqlTypeId fmd of
      SqlTinyIntT -> True
      SqlSmallIntT -> True
      SqlIntegerT -> True
      sqlt | sqlt == SqlDecimalT || sqlt == SqlNumericT ->
          case (fieldScale fmd, fieldSize fmd) of 
            (Just 0,Just size) | size < 9 -> True
            _ -> False
      _ -> False


isLargeIntegral :: FieldMetadata -> Bool
isLargeIntegral fmd = 
    case fieldSqlTypeId fmd of
      SqlBigIntT -> True
      sqlt | sqlt == SqlDecimalT || sqlt == SqlNumericT ->
          case fieldScale fmd of 
            Just 0 -> True
            _ -> False
      _ -> False
          
          
isPreciseDecimal :: FieldMetadata -> Bool
isPreciseDecimal fmd = 
    case fieldSqlTypeId fmd of
      SqlNumericT -> True
      SqlDecimalT -> True
      _ -> False

isSinglePrecisionFloating :: FieldMetadata -> Bool
isSinglePrecisionFloating fmd =
    case fieldSqlTypeId fmd of
      SqlRealT -> True
      _ -> False

isDoublePrecisionFloating :: FieldMetadata -> Bool
isDoublePrecisionFloating fmd =
    case fieldSqlTypeId fmd of
      SqlFloatT -> True  -- sic! Only REAL is single precision in the SQL spec
      SqlDoubleT -> True
      _ -> False

isCharacter :: FieldMetadata -> Bool
isCharacter fmd = 
    case fieldSqlTypeId fmd of
      SqlCharT -> True
      SqlVarCharT -> True
      SqlLongVarCharT -> True
      SqlWCharT -> True
      SqlWVarCharT -> True
      SqlWLongVarCharT -> True
      _ -> False

isBooleanOrBit :: FieldMetadata -> Bool
isBooleanOrBit fmd = 
    case fieldSqlTypeId fmd of
      SqlBitT -> True
      _ -> False

isDate :: FieldMetadata -> Bool
isDate fmd =
    case fieldSqlTypeId fmd of
      SqlDateT -> True
      _ -> False

isTime :: FieldMetadata -> Bool
isTime fmd =
    case fieldSqlTypeId fmd of
      SqlTimeT -> True
      SqlUTCTimeT -> True
      _ -> False

isDateTime :: FieldMetadata -> Bool
isDateTime fmd =
    case fieldSqlTypeId fmd of
      SqlTimestampT -> True
      SqlUTCDateTimeT -> True
      _ -> False



data ForeignKeyConstraint =
    ForeignKeyConstraint
    {
      sourceTable :: TableIdentifier,
      targetTable :: TableIdentifier,
      equatedFields :: [(FieldName,FieldName)]
    }
    deriving (Show)


type FieldName = String
type SchemaName = String




class IConnection conn => IConnectionEx conn where
    
    getTableIdentifiers :: conn -> Maybe SchemaName -> IO [TableIdentifier]
    
    getTableMetadata :: conn -> TableIdentifier -> IO TableMetadata
    getTableMetadata = defaultGetTableMetadata

    getFieldMetadatas :: conn -> TableIdentifier -> IO [FieldMetadata]
    
    getForeignKeyConstraints :: conn -> TableIdentifier -> IO [ForeignKeyConstraint]
    
    getPrimaryKeyFieldNames :: conn -> TableIdentifier -> IO [FieldName]
    

defaultGetTableMetadata :: IConnectionEx conn => conn -> TableIdentifier -> IO TableMetadata
defaultGetTableMetadata conn tableId =
    do
      fieldMds <- getFieldMetadatas conn tableId
      
      pks <- getPrimaryKeyFieldNames conn tableId
                         
      fks <- getForeignKeyConstraints conn tableId

      return $ TableMetadata tableId fieldMds pks fks



getTableMetadatas :: IConnectionEx conn => conn -> Maybe SchemaName -> IO [TableMetadata]
getTableMetadatas conn mSchema =
    do
      tableIds <- getTableIdentifiers conn mSchema
      mapM (getTableMetadata conn) tableIds


-- Testing


printSqlError :: SqlError -> IO ()
printSqlError = print


-- Print all TableMetadatas available on a connection.
printTableMetadatas :: IConnectionEx conn => conn -> IO ()
printTableMetadatas conn =
    catchDyn printMds printSqlError
    where
      printMds = 
          do
            tableMds <- getTableMetadatas conn (Just "public")
            mapM_ (putStrLn . show) tableMds

