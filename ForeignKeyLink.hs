{-# OPTIONS_GHC -fbang-patterns #-}

-- Copyright (c) 2008 Stephen C. Harris.
-- See COPYING file at the root of this distribution for copyright information.


module HMQ.ForeignKeyLink where

import Data.Dynamic
import Data.Typeable

import HMQ.Metadata.TableMetadata(TableMetadata, TableIdentifier, FieldName)
import qualified HMQ.Metadata.TableMetadata as TMD
import HMQ.Query(QueryTable)
import qualified HMQ.Query as Q
import HMQ.RowValueExtractors




-- ForeignKey
-- Represents a foreign key together with extractor functions for the mapped types on both sides.


data ForeignKey a b =

    ForeignKey
    {
      sourceTableMetadata :: TableMetadata,

      targetTableMetadata :: TableMetadata,

      matchedFields :: [(FieldName,FieldName)], -- (foreign key field, primary key field) pairs

      sourceRowValueExtractor :: EntityRowValueExtractor a,

      targetRowValueExtractor :: EntityRowValueExtractor b
    }



-- ForeignKeyLink
-- Represents a ForeignKey along with a left/right sense of traversal.


data ForeignKeyLink a b =
    
    ManyToOne (ForeignKey a b) |
    
    OneToMany (ForeignKey b a)
    


isManyToOne :: ForeignKeyLink a b -> Bool
isManyToOne (ManyToOne _) = True
isManyToOne _ = False


isOneToMany :: ForeignKeyLink a b -> Bool
isOneToMany = not . isManyToOne


leftRowValueExtractor :: ForeignKeyLink a b -> EntityRowValueExtractor a
leftRowValueExtractor (ManyToOne fk) = sourceRowValueExtractor fk
leftRowValueExtractor (OneToMany fk) = targetRowValueExtractor fk
    

rightRowValueExtractor :: ForeignKeyLink a b -> EntityRowValueExtractor b
rightRowValueExtractor (ManyToOne fk) = targetRowValueExtractor fk
rightRowValueExtractor (OneToMany fk) = sourceRowValueExtractor fk
    

leftTable :: ForeignKeyLink a b -> TableIdentifier
leftTable (ManyToOne fk) = TMD.tableIdentifier $ sourceTableMetadata fk
leftTable (OneToMany fk) = TMD.tableIdentifier $ targetTableMetadata fk


rightTable :: ForeignKeyLink a b -> TableIdentifier
rightTable (ManyToOne fk) = TMD.tableIdentifier $ targetTableMetadata fk
rightTable (OneToMany fk) = TMD.tableIdentifier $ sourceTableMetadata fk


leftTableMetadata :: ForeignKeyLink a b -> TableMetadata
leftTableMetadata (ManyToOne fk) = sourceTableMetadata fk
leftTableMetadata (OneToMany fk) = targetTableMetadata fk


rightTableMetadata :: ForeignKeyLink a b -> TableMetadata
rightTableMetadata (ManyToOne fk) = targetTableMetadata fk
rightTableMetadata (OneToMany fk) = sourceTableMetadata fk


matchedFieldsOfLink :: ForeignKeyLink a b -> [(String,String)]
matchedFieldsOfLink (ManyToOne fk) = matchedFields fk
matchedFieldsOfLink (OneToMany fk) = map (\(f1,f2) -> (f2,f1)) (matchedFields fk)


reverseLink :: ForeignKeyLink a b -> ForeignKeyLink b a
reverseLink (ManyToOne fk) = OneToMany fk
reverseLink (OneToMany fk) = ManyToOne fk


equatedMatchedFieldsExpr :: QueryTable -> QueryTable -> ForeignKeyLink a b -> String
equatedMatchedFieldsExpr leftTIQ rightTIQ fkLink = 
    let equateFields (leftField, rightField) = 
            (Q.tableAlias leftTIQ ++ "." ++ leftField) ++ " = " ++ (Q.tableAlias rightTIQ ++ "." ++ rightField)
    in
        foldl
            (\accumExpr fieldPair -> if accumExpr == "" then equateFields fieldPair 
                                     else accumExpr ++ " AND " ++ (equateFields fieldPair))
            ""
            (matchedFieldsOfLink fkLink)


showLink :: ForeignKeyLink a b -> String
showLink fkl =
    TMD.tableIdentifierString (leftTable fkl)    ++ if isManyToOne fkl then " >--" else " --"  ++
    show (map fst $ matchedFieldsOfLink fkl) ++ if isManyToOne fkl then "-- "  else "--< " ++
    TMD.tableIdentifierString (rightTable fkl) 

instance Show (ForeignKeyLink a b) where
    show = showLink
    

type DynamicForeignKeyLink = ForeignKeyLink Dynamic Dynamic


toDynamicForeignKeyLink :: (Typeable a, Typeable b) => ForeignKeyLink a b -> DynamicForeignKeyLink
toDynamicForeignKeyLink fkl = 
    let 
        -- The new extractors apply toDynamic inside the Maybe on the old extractors' results.
        dynamicLeftExtractor !alias !qry = 
            \(!row) -> 
                let val = leftRowValueExtractor fkl alias qry row
                in maybe Nothing (strictJust . toDyn) val
        
        dynamicRightExtractor !alias !qry = 
            \(!row) -> 
                let val = rightRowValueExtractor fkl alias qry row
                in maybe Nothing (strictJust . toDyn) val
    in
      case fkl of

        ManyToOne fk ->
            ManyToOne $ fk { sourceRowValueExtractor = dynamicLeftExtractor, 
                             targetRowValueExtractor = dynamicRightExtractor }
            
        OneToMany fk -> 
            OneToMany $ fk { sourceRowValueExtractor = dynamicRightExtractor,
                             targetRowValueExtractor = dynamicLeftExtractor }
    where
      strictJust !x = Just x

