{-# OPTIONS_GHC -fglasgow-exts -fbang-patterns #-}

-- Copyright (c) 2008 Stephen C. Harris.
-- See COPYING file at the root of this distribution for copyright information.

module HMQ.MappedQuery where

import Data.Maybe
import Data.List
import Data.Char

import Database.HDBC

import HMQ.Query(Query(Query), QueryTable(QueryTable))
import qualified HMQ.Query as Q
import HMQ.ForeignKeyLink
import HMQ.Metadata.TableMetadata(TableMetadata,TableIdentifier)
import qualified HMQ.Metadata.TableMetadata as TMD
import HMQ.RowValueExtractors
import HMQ.Utils.Strings
import HMQ.Utils.Tuples
import HMQ.Utils.Lists






data MappedQuery leftT rightT resultT where


    MappedTable :: TableMetadata -> EntityRowValueExtractor a -> MappedQuery a a a
    

    InnerJoin             :: MappedQuery a b t1 -> ForeignKeyLink b c -> MappedQuery c d t2 -> MappedQuery a d (t1, t2)

    InnerJoinProjectRight :: MappedQuery a b t1 -> ForeignKeyLink b c -> MappedQuery c d t2 -> MappedQuery a d t2

    InnerJoinProjectLeft  :: MappedQuery a b t1 -> ForeignKeyLink b c -> MappedQuery c d t2 -> MappedQuery a d t1


    LeftOuterJoin              :: MappedQuery a b t1 -> ForeignKeyLink b c -> MappedQuery c d t2 -> MappedQuery a d (t1, Maybe t2)

    LeftOuterJoinProjectLeft   :: MappedQuery a b t1 -> ForeignKeyLink b c -> MappedQuery c d t2 -> MappedQuery a d t1

    LeftOuterJoinProjectRight  :: MappedQuery a b t1 -> ForeignKeyLink b c -> MappedQuery c d t2 -> MappedQuery a d (Maybe t2)

    
    RightOuterJoin             :: MappedQuery a b t1 -> ForeignKeyLink b c -> MappedQuery c d t2 -> MappedQuery a d (Maybe t1, t2)

    RightOuterJoinProjectLeft  :: MappedQuery a b t1 -> ForeignKeyLink b c -> MappedQuery c d t2 -> MappedQuery a d (Maybe t1)

    RightOuterJoinProjectRight :: MappedQuery a b t1 -> ForeignKeyLink b c -> MappedQuery c d t2 -> MappedQuery a d t2

    
    FullOuterJoin              :: MappedQuery a b t1 -> ForeignKeyLink b c -> MappedQuery c d t2 -> MappedQuery a d (Maybe t1, Maybe t2)

    FullOuterJoinProjectLeft   :: MappedQuery a b t1 -> ForeignKeyLink b c -> MappedQuery c d t2 -> MappedQuery a d (Maybe t1)

    FullOuterJoinProjectRight  :: MappedQuery a b t1 -> ForeignKeyLink b c -> MappedQuery c d t2 -> MappedQuery a d (Maybe t2)

    -- The following two forms allow exposing the left or right exposed table on the opposite side.  This allows any number of
    -- repeated joins to the same table in the mapped query, allowing multiple branches from the same exposed table.

    ExposeLeftOnRight :: MappedQuery a b t -> MappedQuery a a t

    ExposeRightOnLeft :: MappedQuery a b t -> MappedQuery b b t


-- Some functions to facilitate branching from a common left or right table.

{- Branching from a common left table

  a -- ... -- b    [mq1]
   \[fkl] 
    c -- ... -- d  [mq2]

  Pictured mapped query is branchFromLeft InnerJoin mq1 fkl mq2, of type MappedQuery a d (t1, t2). 
  It exposes a on the left, nothing on the right, and produces (t1,t2) or (t1, Maybe t2), depending on the join type used along the
  foreign key link.  By repeating this operation any number of branches may be added.
-}


-- The first argument is intended to be one of the constructors specifying a join type, e.g. InnerJoin.
branchFromLeft :: (MappedQuery a a t1 -> ForeignKeyLink a c -> MappedQuery c d t2 -> MappedQuery a d t3) -> -- InnerJoin, LeftOuterJoin, etc
                   MappedQuery a b t1 -> ForeignKeyLink a c -> MappedQuery c d t2 -> MappedQuery a d t3
branchFromLeft joinTypeCtr mq1 fkl mq2 =
    joinTypeCtr (ExposeLeftOnRight mq1) fkl mq2


{- Branching from a common right table

   [mq2]    d -- ... --  a
                        / [fkl]
   [mq1]   c -- ... -- b   
-}
branchFromRight :: (MappedQuery c b t1 -> ForeignKeyLink b a -> MappedQuery a a t2 -> MappedQuery c a t3) -> -- InnerJoin, LeftOuterJoin, etc
                    MappedQuery c b t1 -> ForeignKeyLink b a -> MappedQuery d a t2 -> MappedQuery c a t3
branchFromRight joinTypeCtr mq1 fkl mq2 =
    joinTypeCtr mq1 fkl (ExposeRightOnLeft mq2)



{- QueryContext 
     The query context provides context information for a query as part of a larger query.  Currently this is just information about
     the surrounding table aliases, so query-wide unique and predictable table aliases can be created.
-}
data QueryContext =
    
    QueryContext
    {
      preceedingQueryTables :: [QueryTable], -- preceeding aliases already assigned in surrounding query
      
      followingTables :: [TableIdentifier] -- following aliases not yet assigned in surrounding query
    }

emptyQueryContext :: QueryContext
emptyQueryContext = QueryContext [] []



-- MappedQueryProducts
-- The products produced by a mapped query in a given query context, including the SQL query and row value extractor.
data MappedQueryProducts a b t =
    
    MappedQueryProducts
    {
      query :: Query,
               
      queryTables :: [QueryTable],
      
      queryRowValueExtractor :: QueryRowValueExtractor t,
      
      leftExposedQueryTable  :: Maybe QueryTable,
      
      rightExposedQueryTable :: Maybe QueryTable
    }


{- Realizes a mapped query in a query context, producing products such as a SQL query and also a row value extractor for producing
   row values from the query when it is run.
-}
mappedQueryProductsForContext :: QueryContext -> MappedQuery a b t -> MappedQueryProducts a b t
mappedQueryProductsForContext ctx mq = 

    case mq of

      MappedTable tableMetadata entityRowValueExtractor ->

          MappedQueryProducts qry [qt] rowValueExtractor (Just qt) (Just qt)
              
              where
                qt = head $ assignAliases ctx [TMD.tableIdentifier tableMetadata]
                qry = 
                    let
                        fromClause = TMD.tableIdentifierString (Q.tableIdentifier qt) ++ " " ++ Q.tableAlias qt
                        selectFields = makeSelectFields qt tableMetadata
                    in
                      Query selectFields fromClause []
                rowValueExtractor = entityRowValueExtractor (Q.tableAlias qt)


      InnerJoin leftMQ fkl rightMQ ->
          joinProducts ctx leftMQ fkl rightMQ Inner pairLeftRightIfLeft ProjectBoth
 
      InnerJoinProjectLeft leftMQ fkl rightMQ -> 
          joinProducts ctx leftMQ fkl rightMQ Inner selectLeftIfLeft ProjectLeft

      InnerJoinProjectRight leftMQ fkl rightMQ -> 
          joinProducts ctx leftMQ fkl rightMQ Inner selectRightIfRight ProjectRight

      LeftOuterJoin leftMQ fkl rightMQ -> 
          joinProducts ctx leftMQ fkl rightMQ LeftOuter pairLeftMaybeRightIfLeft ProjectBoth

      LeftOuterJoinProjectLeft leftMQ fkl rightMQ -> 
          joinProducts ctx leftMQ fkl rightMQ LeftOuter selectLeftIfLeft ProjectLeft

      LeftOuterJoinProjectRight leftMQ fkl rightMQ -> 
          joinProducts ctx leftMQ fkl rightMQ LeftOuter selectMaybeRightIfLeft ProjectBoth

      RightOuterJoin leftMQ fkl rightMQ -> 
          joinProducts ctx leftMQ fkl rightMQ RightOuter pairMaybeLeftRightIfRight ProjectBoth

      RightOuterJoinProjectLeft leftMQ fkl rightMQ -> 
          joinProducts ctx leftMQ fkl rightMQ RightOuter selectMaybeLeftIfRight ProjectBoth

      RightOuterJoinProjectRight leftMQ fkl rightMQ -> 
          joinProducts ctx leftMQ fkl rightMQ RightOuter selectRightIfRight ProjectRight

      FullOuterJoin leftMQ fkl rightMQ -> 
          joinProducts ctx leftMQ fkl rightMQ FullOuter pairMaybeLeftMaybeRightIfEither ProjectBoth

      FullOuterJoinProjectLeft leftMQ fkl rightMQ -> 
          joinProducts ctx leftMQ fkl rightMQ FullOuter selectMaybeLeftIfEither ProjectBoth

      FullOuterJoinProjectRight leftMQ fkl rightMQ -> 
          joinProducts ctx leftMQ fkl rightMQ FullOuter selectMaybeRightIfEither ProjectBoth

      ExposeLeftOnRight mq -> 
          mqProds { rightExposedQueryTable = leftExposedQueryTable mqProds }
              where
                mqProds = mappedQueryProductsForContext ctx mq

      ExposeRightOnLeft mq -> 
          mqProds { leftExposedQueryTable = rightExposedQueryTable mqProds }
              where
                mqProds = mappedQueryProductsForContext ctx mq




{- joinProducts

   This is the main implementation function for producing mapped query products (such as SQL) from a mapped query in a given query context.
   This function handles those mapped queries which are joins at the top level.

   The postExtraction function argument provides the final forming of the correct result type for the specific type of join from the
   individual results for the two sides.

   The projection argument allows specifying whether the values for the left, right, or both sides should be produced prior to final 
   projection, so unnecessary columns are not part of the underlying SQL query that is produced.
-}

joinProducts :: QueryContext -> MappedQuery a b t1 -> ForeignKeyLink b c -> MappedQuery c d t2 -> 
                JoinType ->  (Maybe t1 -> Maybe t2 -> Maybe t) -> Projection ->
                MappedQueryProducts a d t
joinProducts ctx leftMQ fkl rightMQ joinType resultCombiningFun projection =

    MappedQueryProducts combinedQuery
                        combinedQueryTables
                        combinedRowValueExtractor
                        leftExposedQT
                        rightExposedQT

    where
      
      -- Fetched the table identifiers involved in a mapped query.
            
        leftMQCtx =  QueryContext (preceedingQueryTables ctx) 
                                  (tableIds rightMQ ++ followingTables ctx)
        
        leftMQProds  = mappedQueryProductsForContext leftMQCtx leftMQ
        
        rightMQCtx = QueryContext (preceedingQueryTables ctx ++ queryTables leftMQProds) 
                                  (followingTables ctx)
        rightMQProds = mappedQueryProductsForContext rightMQCtx rightMQ

        leftExposedQT =  leftExposedQueryTable  leftMQProds
        rightExposedQT = rightExposedQueryTable rightMQProds
        
        combinedQueryTables = queryTables leftMQProds ++ queryTables rightMQProds
        
        combinedRowValueExtractor !qry = 
            
            case projection of 
            
              ProjectBoth  ->
                  leftExtractor `seq` rightExtractor `seq`
                        (\(!row) -> resultCombiningFun (leftExtractor  row) (rightExtractor row))

              ProjectLeft  -> 
                    leftExtractor `seq`
                        (\(!row) -> resultCombiningFun (leftExtractor row) Nothing)
                
              ProjectRight ->
                    rightExtractor `seq`
                        (\(!row) -> resultCombiningFun Nothing (rightExtractor row))
            where
              leftExtractor  = queryRowValueExtractor leftMQProds  qry
              rightExtractor = queryRowValueExtractor rightMQProds qry


        combinedQuery :: Query
        combinedQuery =
            case projection of 
              ProjectBoth  -> leftQuery  { Q.selectFields = Q.selectFields leftQuery ++ Q.selectFields rightQuery,
                                           Q.fromClause = combinedFromClause }
              ProjectLeft  -> leftQuery  { Q.fromClause = combinedFromClause }
              ProjectRight -> rightQuery { Q.fromClause = combinedFromClause }
            where
              leftQuery  = query leftMQProds
              rightQuery = query rightMQProds
              
              multLeftQTs  = length (queryTables leftMQProds)  > 1
              multRightQTs = length (queryTables rightMQProds) > 1

              combinedFromClause = (if multLeftQTs then "(" else "") ++ Q.fromClause leftQuery ++ (if multLeftQTs then ")" else "") ++ "\n" ++
                                   joinTypeAsSql joinType ++ "\n" ++ 
                                   (if multRightQTs then "(" else "") ++ Q.fromClause rightQuery ++ (if multRightQTs then ")" else "") ++ "\n" ++
                                   "ON (" ++ equatedMatchedFieldsExpr leftMQJoinQT rightMQJoinQT fkl ++ ")"

              -- The type system should make sure the mapped queries are joinable on their left and right ends 
              -- respectively, so the fromJust's below are justified.
              leftMQJoinQT  = fromJust $ rightExposedQueryTable leftMQProds  -- [sic] *right* exposed qt of *left*  mq
              rightMQJoinQT = fromJust $ leftExposedQueryTable  rightMQProds --       *left*  exposed qt of *right* mq
                

data Projection =
    ProjectLeft  |
    ProjectRight |
    ProjectBoth
    

data JoinType =
    Inner      |
    LeftOuter  |
    RightOuter |
    FullOuter
    deriving (Eq,Show)

joinTypeAsSql :: JoinType -> String
joinTypeAsSql Inner = "INNER JOIN"
joinTypeAsSql LeftOuter = "LEFT JOIN"
joinTypeAsSql RightOuter = "RIGHT JOIN"
joinTypeAsSql FullOuter = "FULL JOIN"




-- Running mapped queries

run :: IConnection conn => conn -> MappedQuery a b t -> IO [t]
run conn mq =
    do
      stmt <- prepare conn (Q.toSqlString qry)
      execute stmt []
      rows <- fetchAllRows stmt
      return $ map rowValueExtractor rows
    where
      mqProds = mappedQueryProductsForContext emptyQueryContext mq
      qry = query mqProds
      -- We're not part of a larger query here, so each row will have a value of type t (never Nothing), so the fromJust is justified.
      rowValueExtractor = fromJust . queryRowValueExtractor mqProds qry

toQuery :: MappedQuery a b t -> Query
toQuery mq = query (mappedQueryProductsForContext emptyQueryContext mq)

toSqlString :: MappedQuery a b t -> String
toSqlString mq = Q.toSqlString $ query $ mappedQueryProductsForContext emptyQueryContext mq


{-  Join result combining functions  
    The following functions are applied after the individual row value extractors for each side of a join operation have yielded their
    left and right Maybe values. Their job is to turn the left and right Maybe values into a single combined value with a type depending
    on the type of join.  The combined value is always a Maybe-wrapped type itself, because any query can be part of a larger mapped query
    in which its entire value is optional on a row-by-row basis, e.g. due to outer joins in the larger query.
-}

pairLeftRightIfLeft :: Maybe t1 -> Maybe t2 -> Maybe (t1,t2)
pairLeftRightIfLeft !maybeLeftVal maybeRightVal = 
    case maybeLeftVal of 
      Just leftVal -> Just $! strict2Tuple leftVal (fromJust maybeRightVal)
      Nothing -> Nothing
    
pairLeftMaybeRightIfLeft :: Maybe t1 -> Maybe t2 -> Maybe (t1,Maybe t2)
pairLeftMaybeRightIfLeft !maybeLeftVal maybeRightVal = 
    case maybeLeftVal of 
      Just leftVal -> Just $! strict2Tuple leftVal maybeRightVal
      Nothing -> Nothing
    
pairMaybeLeftRightIfRight :: Maybe t1 -> Maybe t2 -> Maybe (Maybe t1,t2)
pairMaybeLeftRightIfRight maybeLeftVal !maybeRightVal = 
    case maybeRightVal of 
      Just rightVal -> Just $! strict2Tuple maybeLeftVal rightVal
      Nothing -> Nothing
    
pairMaybeLeftMaybeRightIfEither :: Maybe t1 -> Maybe t2 -> Maybe (Maybe t1,Maybe t2)
pairMaybeLeftMaybeRightIfEither !maybeLeftVal !maybeRightVal = 
    if isJust maybeRightVal || isJust maybeLeftVal then
        Just $! strict2Tuple maybeLeftVal maybeRightVal
    else
        Nothing

selectLeftIfLeft :: Maybe a -> Maybe b -> Maybe a
selectLeftIfLeft !ma mb = ma

selectRightIfRight :: Maybe a -> Maybe b -> Maybe b
selectRightIfRight ma !mb = mb

selectMaybeRightIfLeft :: Maybe a -> Maybe b -> Maybe (Maybe b)
selectMaybeRightIfLeft !l r = if isJust l then Just $! r else Nothing

selectMaybeRightIfEither :: Maybe a -> Maybe b -> Maybe (Maybe b)
selectMaybeRightIfEither l !r = if isJust r || isJust l then Just $! r else Nothing

selectMaybeLeftIfRight :: Maybe a -> Maybe b -> Maybe (Maybe a)
selectMaybeLeftIfRight l !r = if isJust r then Just $! l else Nothing

selectMaybeLeftIfEither :: Maybe a -> Maybe b -> Maybe (Maybe a)
selectMaybeLeftIfEither !l r = if isJust l || isJust r then Just $! l else Nothing




----------------------------------
-- Table alias assignment

assignAliases :: QueryContext -> [TableIdentifier] -> [QueryTable]
assignAliases ctx tbls =
    let
        preceedingContextTables = map Q.tableIdentifier (preceedingQueryTables ctx)
        followingContextTables = followingTables ctx
        
        qtIter accumQTs [] = reverse accumQTs
        qtIter accumQTs (t : remTs) = 
            let 
                prevOccurs =   length (elemIndices t preceedingContextTables)
                               + length (findIndices ((t==) . Q.tableIdentifier) accumQTs)
                occursInRem = elem t remTs || elem t followingContextTables
                    
                alias = tableAliasBaseName t
                        ++ if prevOccurs > 0
                           then show (prevOccurs + 1)
                           else if occursInRem then "1" else ""
            in 
              qtIter (QueryTable t alias : accumQTs) remTs
                   
    in
        qtIter [] tbls


-- Provides default table alias names, before any suffixes to ensure uniqueness are added. 
tableAliasBaseName :: TableIdentifier -> String
tableAliasBaseName table = lowerCase $ TMD.tableName table
   

-- Table alias assignment
----------------------------------


makeSelectFields :: QueryTable -> TableMetadata -> [(String, Maybe String)]
makeSelectFields qt tmd = map fieldExprAndAlias (TMD.fieldMetadatas tmd)
    where
      fieldExprAndAlias fieldMd = 
          ( Q.tableAlias qt ++ "." ++ TMD.fieldName fieldMd,
            Just $ Q.tableAlias qt ++ "/" ++ TMD.fieldName fieldMd )


tableIds mq = map Q.tableIdentifier $ queryTables (mappedQueryProductsForContext emptyQueryContext mq)

