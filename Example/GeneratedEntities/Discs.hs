{-# OPTIONS_GHC -fbang-patterns #-}

module HMQ.Example.GeneratedEntities.Discs where

import Data.List
import Data.Maybe
import System.Time
import Database.HDBC

import HMQ.Metadata.TableMetadata
import HMQ.Query hiding(tableIdentifier)
import HMQ.ForeignKeyLink
import HMQ.MappedQuery
import HMQ.RowValueExtractors



data Disc = 
    Disc
     {
          discId :: String,
          movieId :: Integer,
          rentalDurationDays :: Integer
     }
     deriving (Show)



-- The extractor function which creates a Disc from a result row.
rowValueExtractor ::  EntityRowValueExtractor Disc
rowValueExtractor !tableAlias !qry =
    let
        leadingColIx = fromMaybe (error $ "No columns for table alias " ++ tableAlias ++ " found in query.") $
                                 findIndex ((tableAlias ++ ".") `isPrefixOf`) (selectFieldExprs qry)
    in
      leadingColIx `seq`
          (\(!row) ->
              let
                  fv1:fv2:fv3:_ = drop leadingColIx row
              in
                -- Check first pk field to determine if there's a value for this row
                if fv1 == SqlNull then
                    Nothing
                else
                    Just $ Disc (fromSql fv1) (fromSql fv2) (fromSql fv3)
          )


tableMetadata :: TableMetadata
tableMetadata = 
    TableMetadata {
     tableIdentifier = TableIdentifier {tableSchema = Just "public", tableName = "discs"},
     fieldMetadatas =
       [
           FieldMetadata {fieldName = "disc_id", fieldSqlColDesc = SqlColDesc {colType = SqlCharT, colSize = Just 8, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "movie_id", fieldSqlColDesc = SqlColDesc {colType = SqlBigIntT, colSize = Just 4, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "rental_duration_days", fieldSqlColDesc = SqlColDesc {colType = SqlBigIntT, colSize = Just 4, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}}
       ],
     primaryKeyFieldNames = ["disc_id"],
     foreignKeyConstraints = [ForeignKeyConstraint {sourceTable = TableIdentifier {tableSchema = Just "public", tableName = "discs"}, targetTable = TableIdentifier {tableSchema = Just "public", tableName = "movies"}, equatedFields = [("movie_id","movie_id")]}]
}


mappedQuery :: MappedQuery Disc Disc Disc
mappedQuery = MappedTable tableMetadata rowValueExtractor

