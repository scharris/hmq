{-# OPTIONS_GHC -fbang-patterns #-}

module HMQ.Example.GeneratedEntities.Movies where

import Data.List
import Data.Maybe
import System.Time
import Database.HDBC

import HMQ.Metadata.TableMetadata
import HMQ.Query hiding(tableIdentifier)
import HMQ.ForeignKeyLink
import HMQ.MappedQuery
import HMQ.RowValueExtractors



data Movie = 
    Movie
     {
          movieId :: Integer,
          title :: String,
          categoryId :: Integer,
          movieDuration :: String,
          rating :: String,
          releaseDate :: CalendarTime
     }
     deriving (Show)



-- The extractor function which creates a Movie from a result row.
rowValueExtractor ::  EntityRowValueExtractor Movie
rowValueExtractor !tableAlias !qry =
    let
        leadingColIx = fromMaybe (error $ "No columns for table alias " ++ tableAlias ++ " found in query.") $
                                 findIndex ((tableAlias ++ ".") `isPrefixOf`) (selectFieldExprs qry)
    in
      leadingColIx `seq`
          (\(!row) ->
              let
                  fv1:fv2:fv3:fv4:fv5:fv6:_ = drop leadingColIx row
              in
                -- Check first pk field to determine if there's a value for this row
                if fv1 == SqlNull then
                    Nothing
                else
                    Just $ Movie (fromSql fv1) (fromSql fv2) (fromSql fv3) (fromSql fv4) (fromSql fv5) (fromSql fv6)
          )


tableMetadata :: TableMetadata
tableMetadata = 
    TableMetadata {
     tableIdentifier = TableIdentifier {tableSchema = Just "public", tableName = "movies"},
     fieldMetadatas =
       [
           FieldMetadata {fieldName = "movie_id", fieldSqlColDesc = SqlColDesc {colType = SqlBigIntT, colSize = Just 4, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "title", fieldSqlColDesc = SqlColDesc {colType = SqlVarCharT, colSize = Just 80, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "category_id", fieldSqlColDesc = SqlColDesc {colType = SqlBigIntT, colSize = Just 4, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "movie_duration", fieldSqlColDesc = SqlColDesc {colType = SqlVarCharT, colSize = Just 10, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "rating", fieldSqlColDesc = SqlColDesc {colType = SqlVarCharT, colSize = Just 10, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}},
           FieldMetadata {fieldName = "release_date", fieldSqlColDesc = SqlColDesc {colType = SqlDateT, colSize = Just 4, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False}}
       ],
     primaryKeyFieldNames = ["movie_id"],
     foreignKeyConstraints = [ForeignKeyConstraint {sourceTable = TableIdentifier {tableSchema = Just "public", tableName = "movies"}, targetTable = TableIdentifier {tableSchema = Just "public", tableName = "categories"}, equatedFields = [("category_id","category_id")]}]
}


mappedQuery :: MappedQuery Movie Movie Movie
mappedQuery = MappedTable tableMetadata rowValueExtractor

