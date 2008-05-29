-- Copyright (c) 2008 Stephen C. Harris.
-- See COPYING file at the root of this distribution for copyright information.

module HMQ.Metadata.Sqlite3 where

import Control.Monad
import Control.Exception
import Text.Regex
import Data.Maybe
import Database.HDBC
import qualified Database.HDBC.Sqlite3 as S3


import qualified HMQ.Metadata.IConnectionEx as IC

import HMQ.Metadata.TableMetadata
import qualified HMQ.Metadata.TableMetadata as TMD

import HMQ.Utils.Strings



instance IC.IConnectionEx S3.Connection where
    
    getTableIdentifiers      = getTableIdentifiers
    
    getFieldMetadatas        = getFieldMetadatas
    
    getForeignKeyConstraints = getForeignKeyConstraints
    
    getPrimaryKeyFieldNames  = getPrimaryKeyFieldNames



-- The schema name is ignored as Sqlite does not support namespaces for table names within a database.
getTableIdentifiers :: S3.Connection -> Maybe SchemaName -> IO [TableIdentifier]
getTableIdentifiers conn mSchema =
    do
      tableNames <- getTables conn

      return $ map (TableIdentifier Nothing) tableNames


getFieldMetadatas :: S3.Connection -> TableIdentifier -> IO [FieldMetadata]
getFieldMetadatas conn tableId =
    undefined -- argh!! This and the rest require parsing the contents of sqlite_master.sql column (which is the full CREATE TABLE text).
                                       


getForeignKeyConstraints ::  S3.Connection -> TableIdentifier -> IO [ForeignKeyConstraint]
getForeignKeyConstraints conn tableId =
    undefined

getPrimaryKeyFieldNames  :: S3.Connection -> TableIdentifier -> IO [FieldName]
getPrimaryKeyFieldNames conn tableId =
    undefined


