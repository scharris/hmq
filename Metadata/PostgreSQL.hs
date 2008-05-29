-- Copyright (c) 2008 Stephen C. Harris.
-- See COPYING file at the root of this distribution for copyright information.

module HMQ.Metadata.PostgreSQL where

import Control.Monad
import Control.Exception
import Text.Regex
import Data.Maybe
import Database.HDBC
import qualified Database.HDBC.PostgreSQL as PG

import HMQ.Metadata.TableMetadata(TableIdentifier(..),FieldMetadata(..),ForeignKeyConstraint(..),fieldName,FieldName,SchemaName)
import qualified HMQ.Metadata.TableMetadata as TMD

import HMQ.Utils.Strings




instance TMD.IConnectionEx PG.Connection where
    
    getTableIdentifiers      = getTableIdentifiers
    
    getFieldMetadatas        = getFieldMetadatas
    
    getForeignKeyConstraints = getForeignKeyConstraints
    
    getPrimaryKeyFieldNames  = getPrimaryKeyFieldNames




getTableIdentifiers :: PG.Connection -> Maybe SchemaName -> IO [TableIdentifier]
getTableIdentifiers conn mSchema =
    do
      stmt <- prepare conn qry

      execute stmt params

      tableIdRows <- fetchAllRows stmt

      return $ map (\[schema,table] -> TableIdentifier (fromSql schema) (fromSql table)) tableIdRows
    
    where
      qry = "SELECT ns.nspname, cl.relname FROM pg_class cl INNER JOIN pg_namespace ns ON (ns.oid = cl.relnamespace)\n"
            ++ "WHERE cl.relkind = 'r'" ++ (if isJust mSchema then " AND ns.nspname = ?" else "")
      params = if isJust mSchema then [toSql $ fromJust mSchema] else [] 



getFieldMetadatas :: PG.Connection -> TableIdentifier -> IO [FieldMetadata]
getFieldMetadatas conn tableId =
    do 
      stmt <- prepare conn qry

      execute stmt params

      rows <- fetchAllRows stmt
      
      return $ map makeFieldMdForRow rows

    where
      tableName = TMD.tableName tableId
      mSchema = TMD.tableSchema tableId

      qry = unlines ["SELECT a.attname, a.atttypid, a.attlen, format_type(a.atttypid, a.atttypmod), a.attnotnull",
                     "FROM pg_attribute a",
                     "INNER JOIN pg_class cl ON (a.attrelid = cl.oid)",
                     "INNER JOIN pg_namespace ns ON (cl.relnamespace = ns.oid)",
                     "WHERE a.attnum > 0 and a.attisdropped IS FALSE",
                     "AND cl.relname = ?" ++ (if isJust mSchema then " AND ns.nspname = ?" else ""),
                     "ORDER BY a.attnum"]
      
      params = toSql tableName : if isJust mSchema then [toSql $ fromJust mSchema] else [] 

      makeFieldMdForRow :: [SqlValue] -> FieldMetadata
      makeFieldMdForRow [attname, atttypid, attlen, formattedtype, attnotnull] =
          let
              coltype = atttypeidToSqlTypeId (fromSql atttypid)

              size = case fromSql attlen of
                          -1 -> maybeExtractFirstParenthesizedNumber (fromSql formattedtype)
                          x -> Just x

              decDigs = if coltype == SqlNumericT then maybeExtractSecondParenthesizedNumber (fromSql formattedtype)
                        else Nothing
          in
            FieldMetadata 
            { 
              fieldName = fromSql attname,
                
              fieldSqlColDesc = 
                  SqlColDesc 
                  {
                    colType = coltype,
                    colSize = size,
                    colOctetLength = Nothing, -- not available in postgres
                    colDecDigits = decDigs,
                    colNullable = Just (fromSql attnotnull == 'f')
                  }
            }

      maybeExtractFirstParenthesizedNumber :: String -> Maybe Int
      maybeExtractFirstParenthesizedNumber formattedType = 
          let matches = matchRegexAll (mkRegex "\\( *([0-9]+) *[,)]") formattedType
          in case matches of 
               Just (_,_,_, n:_) -> Just $ read n
               _ -> Nothing            
          
      maybeExtractSecondParenthesizedNumber :: String -> Maybe Int
      maybeExtractSecondParenthesizedNumber formattedType = 
          let matches = matchRegexAll (mkRegex "\\( *[0-9]+ *, *([0-9]+) *[,)]") formattedType
          in case matches of 
               Just (_,_,_, n:_) -> Just $ read n
               _ -> Nothing            

      atttypeidToSqlTypeId :: Integer -> SqlTypeId
      atttypeidToSqlTypeId oid =
          if      oid == pgtype_CHAR then SqlCharT
          else if oid == pgtype_CHAR2 then SqlCharT
          else if oid == pgtype_CHAR4 then SqlCharT
          else if oid == pgtype_CHAR8 then SqlCharT
          else if oid == pgtype_NAME then SqlVarCharT
          else if oid == pgtype_BPCHAR then SqlCharT
          else if oid == pgtype_VARCHAR then SqlVarCharT
          else if oid == pgtype_TEXT then SqlVarCharT
          else if oid == pgtype_BYTEA then SqlVarBinaryT
          else if oid == pgtype_INT2 then SqlSmallIntT
          else if oid == pgtype_OID then SqlIntegerT
          else if oid == pgtype_XID then SqlIntegerT
          else if oid == pgtype_INT4 then SqlBigIntT
          else if oid == pgtype_INT8 then SqlBigIntT
          else if oid == pgtype_NUMERIC then SqlNumericT
          else if oid == pgtype_FLOAT4 then SqlRealT
          else if oid == pgtype_FLOAT8 then SqlFloatT
          else if oid == pgtype_DATE then SqlDateT
          else if oid == pgtype_ABSTIME then SqlTimestampT
          else if oid == pgtype_DATETIME then SqlTimestampT
          else if oid == pgtype_TIMESTAMP_NO_TMZONE then SqlTimestampT
          else if oid == pgtype_TIMESTAMP then SqlTimestampT
          else if oid == pgtype_TIME then SqlTimeT
          else if oid == pgtype_TIME_WITH_TMZONE then SqlTimeT
          else if oid == pgtype_TINTERVAL then SqlIntervalT SqlIntervalMonthT -- SqlIntervalMonthT chosen arbitrarily in these two. PG allows any parts
          else if oid == pgtype_RELTIME   then SqlIntervalT SqlIntervalMonthT -- of an interval (microsecond to millennium) to be specified together.
          else if oid == pgtype_BOOL then SqlBitT
          else SqlUnknownT (show oid)
              where     
                pgtype_BOOL =                    16
                pgtype_BYTEA =                   17
                pgtype_CHAR =                    18
                pgtype_NAME =                    19
                pgtype_INT8 =                    20
                pgtype_INT2 =                    21
                pgtype_INT2VECTOR =              22
                pgtype_INT4 =                    23
                pgtype_REGPROC =                 24
                pgtype_TEXT =                    25
                pgtype_OID =                     26
                pgtype_TID =                     27
                pgtype_XID =                     28
                pgtype_CID =                     29
                pgtype_OIDVECTOR =               30
                pgtype_SET =                     32
                pgtype_CHAR2 =                   409
                pgtype_CHAR4 =                   410
                pgtype_CHAR8 =                   411
                pgtype_POINT =                   600
                pgtype_LSEG =                    601
                pgtype_PATH =                    602
                pgtype_BOX =                     603
                pgtype_POLYGON =                 604
                pgtype_FILENAME =                605
                pgtype_FLOAT4 =                  700
                pgtype_FLOAT8 =                  701
                pgtype_ABSTIME =                 702
                pgtype_RELTIME =                 703
                pgtype_TINTERVAL =               704
                pgtype_UNKNOWN =                 705
                pgtype_MONEY =                   790
                pgtype_OIDINT2 =                 810
                pgtype_OIDINT4 =                 910
                pgtype_OIDNAME =                 911
                pgtype_BPCHAR =                  1042
                pgtype_VARCHAR =                 1043
                pgtype_DATE =                    1082
                pgtype_TIME =                    1083
                pgtype_TIMESTAMP_NO_TMZONE =     1114
                pgtype_DATETIME =                1184
                pgtype_TIME_WITH_TMZONE =        1266
                pgtype_TIMESTAMP =               1296
                pgtype_NUMERIC =                 1700
                                       


getForeignKeyConstraints ::  PG.Connection -> TableIdentifier -> IO [ForeignKeyConstraint]
getForeignKeyConstraints conn tableId =
    do
      stmt <- prepare conn qry

      execute stmt params

      rows <- fetchAllRows stmt
      
      mapM getFKCForRow rows

    where
      qry = unlines $
        [
         "SELECT con.conrelid AS srctableid, con.conkey AS srcfieldnums, ",
         "tgtns.nspname AS tgtns, tgt.relname AS tgttable, con.confrelid AS tgttableid, con.confkey AS tgtfieldnums",
         "FROM pg_constraint con",
         "INNER JOIN pg_class src ON (con.conrelid = src.oid)",
         "INNER JOIN pg_class tgt ON (con.confrelid = tgt.oid)",
         "INNER JOIN pg_namespace srcns ON (src.relnamespace = srcns.oid)",
         "INNER JOIN pg_namespace tgtns ON (tgt.relnamespace = tgtns.oid)",
         "WHERE con.contype = 'f'",
         "AND src.relname = ?" ++ (if isJust mSchema then " AND srcns.nspname = ?" else "")
        ]
      
      params = toSql tableName : if isJust mSchema then [toSql $ fromJust mSchema] else [] 

      getFKCForRow :: [SqlValue] -> IO ForeignKeyConstraint
      getFKCForRow (srcTableOid : srcAttNumsStr : tgtSchema : tgtTableName : tgtTableOid : tgtAttNumsStr : []) =
          do
            srcFieldNames <- getFieldNames conn (fromSql srcTableOid) (pgsqlIntArrayStringToList $ fromSql srcAttNumsStr)
            
            tgtFieldNames <- getFieldNames conn (fromSql tgtTableOid) (pgsqlIntArrayStringToList $ fromSql tgtAttNumsStr)
            
            return $ ForeignKeyConstraint tableId 
                                          (TableIdentifier (fromSql tgtSchema) (fromSql tgtTableName))
                                          (zip srcFieldNames tgtFieldNames)
      tableName = TMD.tableName tableId
      mSchema = TMD.tableSchema tableId

      

getPrimaryKeyFieldNames  :: PG.Connection -> TableIdentifier -> IO [FieldName]
getPrimaryKeyFieldNames conn tableId =

    do
      stmt <- prepare conn qry

      execute stmt params

      rows <- fetchAllRows stmt
      
      if not $ null rows 
        then getPKForRow (head rows) -- we're only interested in one pk (which may be multiple fields)
        else return []

    where
      qry = unlines $
        [
         "SELECT con.conrelid AS tableid, con.conkey AS fieldnums",
         "FROM pg_constraint con",
         "INNER JOIN pg_class cl ON (con.conrelid = cl.oid)",
         "INNER JOIN pg_namespace ns ON (cl.relnamespace = ns.oid)",
         "WHERE con.contype = 'p'",
         "AND cl.relname = ?" ++ (if isJust mSchema then " AND ns.nspname = ?" else "")
        ]
      
      params = toSql tableName : if isJust mSchema then [toSql $ fromJust mSchema] else [] 

      getPKForRow :: [SqlValue] -> IO [FieldName]
      getPKForRow (tableOid : fieldNumsStr : []) =
          getFieldNames conn (fromSql tableOid) (pgsqlIntArrayStringToList $ fromSql fieldNumsStr)

      tableName = TMD.tableName tableId
      mSchema = TMD.tableSchema tableId



pgsqlIntArrayStringToList :: String -> [Int]
pgsqlIntArrayStringToList s =
  let 
      listStr = map (\c -> case c of '{' -> '['; '}' -> ']'; _ -> c) s
  in
    read listStr


getFieldNames :: PG.Connection -> Integer -> [Int] -> IO [String]
getFieldNames conn srcTableOid attNums =
    do
      attNames <- quickQuery conn attsQry []
      
      return $ map (fromSql . head) attNames

    where
      attsQry = "SELECT attname FROM pg_attribute WHERE attrelid = " ++ show srcTableOid 
                ++ " AND attnum IN  (" ++ listAsString show "," attNums ++ ")"




-- For testing


testConnStr = "dbname='testdb' user='sharris' password='anna'"

printMds =
    catchDyn
      (do
        conn <- PG.connectPostgreSQL testConnStr
        TMD.printTableMetadatas conn
      )
      printSqlError
    where
      printSqlError :: SqlError -> IO ()
      printSqlError se = do { print se }


describe table =
    catchDyn doQry printSqlError
    where
      doQry = do
        conn <- PG.connectPostgreSQL testConnStr
        describeTable conn table
      
      printSqlError :: SqlError -> IO [(String,SqlColDesc)]
      printSqlError se = do { print se; return [] }

