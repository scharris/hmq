{-# OPTIONS_GHC -fbang-patterns #-}

-- Copyright (c) 2008 Stephen C. Harris.
-- See COPYING file at the root of this distribution for copyright information.

module HMQ.ReverseEngineering where


import System.IO
import System.FilePath
import System.Directory
import System.Time
import Text.Regex
import Data.Maybe
import Data.List
import Data.Typeable
import Data.Set(Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

import HMQ.Utils.Strings
import HMQ.ForeignKeyLink
import HMQ.Metadata.TableMetadata(TableMetadata, TableIdentifier, FieldMetadata, ForeignKeyConstraint)
import qualified HMQ.Metadata.TableMetadata as TMD
import HMQ.MappedQuery


{- TODO:

     And child''parent and parent''child when there's only one fk between them

-}


-- Generates entity source files for a given schema pattern.
-- Returns the count of written entity source files.
generateEntityModuleSourceFiles :: Options -> FilePath -> [TableMetadata] -> IO ()
generateEntityModuleSourceFiles opts outputDir []        = return ()
generateEntityModuleSourceFiles opts outputDir (tmd: tmds) =

    do
      createDirectoryIfMissing True (takeDirectory outputFile)

      writeFile outputFile moduleSrc

      generateEntityModuleSourceFiles opts outputDir tmds

    where
      outputFile = outputDir ++ "/" ++ subRegex (mkRegex "\\.") moduleQName "/" ++ ".hs"

      moduleSrc = entityModuleSourceCode moduleDef

      moduleQName = entityModuleQName moduleDef  

      moduleDef = entityModuleDefinition opts tmd
      


entityModuleSourceCode :: EntityModuleDefinition -> String
entityModuleSourceCode modDef = 
     "{-# OPTIONS_GHC -fbang-patterns #-}\n\n"
     ++ "module " ++ entityModuleQName modDef ++ " where"
     ++ "\n\n"
     ++ unlines (entityModuleModuleImports modDef)
     ++ "\n\n\n"
     ++ entityModuleEntityDefinition modDef
     ++ "\n\n"
     ++ entityModuleExtractorDefinition modDef
     ++ "\n\n"
     ++ entityModuleTableMetadataDefinition modDef
     ++ "\n\n"
     ++ entityModuleMappedQueryDefinition modDef
     ++ "\n"


entityModuleDefinition :: Options -> TableMetadata -> EntityModuleDefinition
entityModuleDefinition opts tableMd = 
    
    EntityModuleDefinition 
    { 
      entityModuleQName = moduleQName, 
      entityModuleTableMetadata = tableMd,
      entityModuleEntityName = entityName,
      entityModuleModuleImports = entityModuleImports opts,
      entityModuleEntityDefinition = makeEntityDefinition entityName fieldDefs,
      entityModuleExtractorDefinition = makeEntityRowValueExtractorDefinition opts entityName tableMd,
      entityModuleTableMetadataDefinition = makeEntityTableMetadataDefinition opts tableMd,
      entityModuleMappedQueryDefinition = makeEntityMappedQueryDefinition entityName
    }
    
    where

      tableId = TMD.tableIdentifier tableMd
      
      moduleQName = outputModulesQualifiedNamesPrefix opts ++ entityModuleNamingFunction opts tableId  
      
      entityName = entityNamingFunction opts tableId
      
      fieldDefs = map (fieldDefinitionFunction opts) (TMD.fieldMetadatas tableMd)



----------------------------------------------------------------------
-- Functions for generating the parts of the generated entity modules


makeEntityDefinition :: String -> [EntityFieldDefinition] -> String
makeEntityDefinition entityName fieldDefs =
    unlines [ "data " ++ entityName ++ " = ",
              "    " ++ entityName,
              "     {",
              listAsString fieldDefLine ",\n" fieldDefs,
              "     }",
              "     deriving (Show)",
              ""
            ]
    where
      fieldDefLine efd = "          " ++ entityFieldName efd ++ " :: " ++ entityFieldTypeString efd
          


makeEntityRowValueExtractorDefinition :: Options -> String -> TableMetadata -> String
makeEntityRowValueExtractorDefinition opts entityName tableMd = 
    
     unlines [
            "-- The extractor function which creates " ++ indefiniteArticleFor entityName ++ " " ++ entityName ++ " from a result row.",
            "rowValueExtractor ::  EntityRowValueExtractor " ++ entityName,
            "rowValueExtractor !tableAlias !qry =", 
            "    let",
            "        leadingColIx = fromMaybe (error $ \"No columns for table alias \" ++ tableAlias ++ \" found in query.\") $",
            "                                 findIndex ((tableAlias ++ \".\") `isPrefixOf`) (selectFieldExprs qry)", 
            "    in",
            "      leadingColIx `seq`",
            "          (\\(!row) ->",
            "              let",
            "                  " ++ listAsString (("fv" ++) . show) ":" fieldNums ++ ":_ = drop leadingColIx row",
            "              in",
            "                -- Check first pk field to determine if there's a value for this row",
            "                if fv" ++ show firstPkFieldRelativeColNum ++ " == SqlNull then",
            "                    Nothing",
            "                else",
            "                    Just $ " ++ entityName ++ " " ++ listAsString (\fNum -> "(fromSql fv" ++ show fNum ++ ")") " " fieldNums,
            "          )"
     ]
     where 
       fieldNums = [1..(length $ TMD.fieldMetadatas tableMd)]
       
       firstPkFieldRelativeColNum = 
           if not (null pks) then
               1 + fromMaybe (error $ "Primary key field " ++ head pks ++ " not found in field metadatas for table " ++ tableName ++ ".") 
                             (elemIndex (head pks) (TMD.fieldNames tableMd))
           else 
               error $ "Table " ++ tableName  ++ " has no primary key: row value extractors require a primary key."
       pks = TMD.primaryKeyFieldNames tableMd
       tableName = TMD.tableIdentifierString (TMD.tableIdentifier tableMd)


makeEntityTableMetadataDefinition :: Options -> TableMetadata -> String
makeEntityTableMetadataDefinition opts tableMd = 
    "tableMetadata :: TableMetadata\n"
    ++ "tableMetadata = \n" 
    ++ "    " ++ show tableMd ++ "\n"


makeEntityMappedQueryDefinition :: String -> String
makeEntityMappedQueryDefinition entityName = 
    "mappedQuery :: MappedQuery " ++ entityName ++ " " ++ entityName ++ " " ++ entityName ++ "\n"
    ++ "mappedQuery = MappedTable tableMetadata rowValueExtractor\n"
    

-- Functions for generating the parts of the generated entity modules
----------------------------------------------------------------------



-------------------------------------------
-- Foreign key module generation


generateForeignKeyLinksModuleSourceFile :: Options -> String -> [TableMetadata] -> IO ()
generateForeignKeyLinksModuleSourceFile opts outputDir tableMd =
    do
      createDirectoryIfMissing True parentDir

      writeFile outputFile moduleSrc

    where
      parentDir = takeDirectory outputFile

      outputFile = outputDir ++ "/" ++ subRegex (mkRegex "\\.") moduleQName "/" ++ ".hs"

      moduleQName = outputModulesQualifiedNamesPrefix opts ++ foreignKeyLinksModuleName opts
            
      moduleSrc = foreignKeyLinksModuleSourceCode moduleDef

      moduleDef = foreignKeyLinksModuleDefinition opts tableMd



foreignKeyLinksModuleDefinition :: Options -> [TableMetadata] -> ForeignKeyLinksModuleDefinition
foreignKeyLinksModuleDefinition opts tableMds =
    
    foldl' (addDefsForFkc opts) emptyFkLinksModule fkcs
    
    where
      
      fkcs = concatMap TMD.foreignKeyConstraints tableMds

      moduleQName = outputModulesQualifiedNamesPrefix opts ++ foreignKeyLinksModuleName opts

      emptyFkLinksModule = ForeignKeyLinksModuleDefinition moduleQName fixedImports Set.empty Set.empty [] []

      fixedImports = ["import Data.Dynamic",
                      "",
                      "import HMQ.ForeignKeyLink"]

      -- Add link definitions for a foreign key constraint
      addDefsForFkc :: Options -> ForeignKeyLinksModuleDefinition -> ForeignKeyConstraint -> ForeignKeyLinksModuleDefinition
      addDefsForFkc !opts !fkMod !fkc =
          
          ForeignKeyLinksModuleDefinition
          {
            fksModuleQName = moduleQName,
            fksModuleFixedImports = fixedImports,
            fksModuleEntityImports = entityModuleImports,
            fksModuleEntityMappedQueries = entityMappedQueries,
            fksModuleLinkDefs = linkDefs,
            fksModuleDynamicLinkDefs = dynamicLinkDefs
          }
      
          where
            srcTable = TMD.sourceTable fkc
            tgtTable = TMD.targetTable fkc    
            
            srcModuleQName = outputModulesQualifiedNamesPrefix opts ++ entityModuleNamingFunction opts srcTable
            tgtModuleQName = outputModulesQualifiedNamesPrefix opts ++ entityModuleNamingFunction opts tgtTable
            
            
            -- The many-one link

            manyOneLinkName = manyToOneForeignKeyLinkNamingFunction opts fkc
            
            eqFieldsLiteral = "[" ++ listAsString (\(f1,f2) -> "(" ++ quote f1 ++ "," ++ quote f2 ++ ")") "," (TMD.equatedFields fkc) ++ "]"
            
            manyOneLinkDef = 
                manyOneLinkName ++ " =\n" 
                ++ "    ManyToOne\n" 
                ++ "        (ForeignKey\n" 
                ++ "            " ++ srcModuleQName ++ ".tableMetadata\n"
                ++ "            " ++ tgtModuleQName ++ ".tableMetadata\n"
                ++ "            " ++ eqFieldsLiteral ++ "\n" 
                ++ "            " ++ srcModuleQName ++ ".rowValueExtractor\n"
                ++ "            " ++ tgtModuleQName ++ ".rowValueExtractor\n"
                ++ "        )"


            -- The one-many link

            oneManyLinkName = oneToManyForeignKeyLinkNamingFunction opts fkc
            
            oneManyLinkDef = oneManyLinkName ++ " = reverseLink " ++ manyOneLinkName


            linkDefs = manyOneLinkDef : oneManyLinkDef : fksModuleLinkDefs fkMod


            -- Dynamic links
                    
            dynamicManyOneLink = "toDynamicForeignKeyLink " ++ manyOneLinkName
            dynamicOneManyLink = "toDynamicForeignKeyLink " ++ oneManyLinkName

            dynamicLinkDefs = dynamicManyOneLink : dynamicOneManyLink : fksModuleDynamicLinkDefs fkMod    
                
                            
            entityModuleImports = Set.insert ("import qualified " ++ srcModuleQName)
                                             (Set.insert ("import qualified " ++ tgtModuleQName) 
                                                         (fksModuleEntityImports fkMod))

            entityMappedQueries = Set.insert (tableAliasBaseName srcTable ++ " = " ++ srcModuleQName ++ ".mappedQuery\n")
                                             (Set.insert (tableAliasBaseName tgtTable ++ " = " ++ tgtModuleQName ++ ".mappedQuery\n") 
                                                         (fksModuleEntityMappedQueries fkMod))


      
      
foreignKeyLinksModuleSourceCode :: ForeignKeyLinksModuleDefinition -> String
foreignKeyLinksModuleSourceCode fkmod =
    "module " ++ fksModuleQName fkmod ++ " where" 
    ++ "\n\n"
    ++ unlines (fksModuleFixedImports fkmod)
    ++ "\n"
    ++ unlines (Set.toList $ fksModuleEntityImports fkmod)
    ++ "\n\n\n"
    ++ " -- Entity module mapped queries\n"
    ++ unlines (Set.toList $ fksModuleEntityMappedQueries fkmod)
    ++ "\n\n\n"
    ++ unlines (intersperse "\n" $ fksModuleLinkDefs fkmod)
    ++ "\n\n\n"
       -- Dynamic link definitions are commented out for now, because CalendarTime isn't in class Typeable, so the entities with dates/times are Typeable.
    ++ "{- Dynamic representation of all foreign key links, both many-one and one-many.\n"
    ++ "dynamicForeignKeyLinks :: [ForeignKeyLink Dynamic Dynamic]\n"
    ++ "dynamicForeignKeyLinks =\n" 
    ++ "    [\n"
    ++ listAsString (indent 8) ",\n" (fksModuleDynamicLinkDefs fkmod)
    ++ "\n"
    ++ "    ]\n-}\n"
    

-- Foreign key module generation
-------------------------------------------



-----------------
-- Defaults

defaultOptions = Options {
    
    entityModuleNamingFunction =  defaultEntityModuleNamingFunction :: TableIdentifier -> String,

    foreignKeyLinksModuleName = "GeneratedForeignKeyLinks",
    
    outputModulesQualifiedNamesPrefix = "",  -- Prepended to the above to form the final qualified output module names
    
    
    entityNamingFunction = defaultEntityNamingFunction :: TableIdentifier -> String,

    fieldDefinitionFunction = defaultFieldDefinitionFunction :: FieldMetadata -> EntityFieldDefinition,
    
    entityModuleImports = defaultEntityModuleImports,
    
  
    manyToOneForeignKeyLinkNamingFunction = defaultManyToOneForeignKeyLinkNamingFunction,
                
    oneToManyForeignKeyLinkNamingFunction = defaultOneToManyForeignKeyLinkNamingFunction
}


defaultEntityModuleNamingFunction :: TableIdentifier -> String
defaultEntityModuleNamingFunction = ("GeneratedEntities." ++) . capitalize . lowerCase . TMD.tableName
    

defaultEntityNamingFunction :: TableIdentifier -> String
defaultEntityNamingFunction =  camelCase True . singularize . TMD.tableName
   

defaultFieldDefinitionFunction :: FieldMetadata -> EntityFieldDefinition
defaultFieldDefinitionFunction fieldMd = 

    EntityFieldDefinition { entityFieldName = fieldName, entityFieldTypeString =  typeStr }

    where
      fieldName = camelCase False $ TMD.fieldName fieldMd
      typeStr = standardTypeStringForField fieldMd
      
      standardTypeStringForField :: FieldMetadata -> String
      standardTypeStringForField fieldMd =

          let baseType = if TMD.isSmallIntegral fieldMd then "Int"
                         else if TMD.isLargeIntegral fieldMd then "Integer"
                         else if TMD.isPreciseDecimal fieldMd then "Rational"
                         else if TMD.isSinglePrecisionFloating fieldMd then "Float"
                         else if TMD.isDoublePrecisionFloating fieldMd then "Double"
                         else if TMD.isCharacter fieldMd then "String"
                         else if TMD.isBooleanOrBit fieldMd then "Bool"
                         else if TMD.isDate fieldMd || TMD.isDateTime fieldMd then "CalendarTime"
                         else if TMD.isTime fieldMd then "TimeDiff"
                         else
                             error $ "Field type of field " ++ show fieldMd ++ " is currently not supported."
              nullable =  fromMaybe True (TMD.fieldIsNullable fieldMd)
              maybeWrap tstr = if nullable then "Maybe " ++ tstr else tstr
          in
            maybeWrap baseType
    

defaultEntityModuleImports :: [String]
defaultEntityModuleImports =
    [
        "import Data.List",
        "import Data.Maybe",
        "import System.Time",
        "import Database.HDBC",
        "",
        "import HMQ.Metadata.TableMetadata",
        "import HMQ.Query hiding(tableIdentifier)",
        "import HMQ.ForeignKeyLink",
        "import HMQ.MappedQuery",
        "import HMQ.RowValueExtractors"
    ]
    

defaultManyToOneForeignKeyLinkNamingFunction :: ForeignKeyConstraint -> String
defaultManyToOneForeignKeyLinkNamingFunction fkc = 
    tableAliasBaseName srcTable ++ "'" ++ concatenatedFkFields
    where
      srcTable = TMD.sourceTable fkc
      concatenatedFkFields = listAsString (lowerCase . fst) "_" (TMD.equatedFields fkc)
    

defaultOneToManyForeignKeyLinkNamingFunction :: ForeignKeyConstraint -> String
defaultOneToManyForeignKeyLinkNamingFunction fkc = 
    tableAliasBaseName tgtTable ++ "''" ++ tableAliasBaseName srcTable ++ "'" ++ concatenatedFkFields
    where
      tgtTable = TMD.targetTable fkc
      srcTable = TMD.sourceTable fkc
      concatenatedFkFields = listAsString (lowerCase . snd) "_" (TMD.equatedFields fkc)
    

          

-- Defaults
-----------------


----------------------------------------
-- Auxiliary types and related functions
----------------------------------------


data Options =
    Options
    { 
      entityModuleNamingFunction :: TableIdentifier -> String,
                
      foreignKeyLinksModuleName :: String,
                
      outputModulesQualifiedNamesPrefix :: String, -- Prepended to the above to form the final qualified module names
                
                
      entityNamingFunction :: TableIdentifier -> String,
                
      fieldDefinitionFunction :: FieldMetadata -> EntityFieldDefinition,
                
      entityModuleImports :: [String],
                
                             
      manyToOneForeignKeyLinkNamingFunction :: ForeignKeyConstraint -> String,
                
      oneToManyForeignKeyLinkNamingFunction :: ForeignKeyConstraint -> String
    }



data EntityModuleDefinition =
    
    EntityModuleDefinition
    {        
        entityModuleQName :: String,
        
        entityModuleTableMetadata :: TableMetadata,
        
        entityModuleEntityName :: String,
        
        entityModuleModuleImports :: [String],
        
        entityModuleEntityDefinition :: String,
        
        entityModuleExtractorDefinition :: String,
        
        entityModuleTableMetadataDefinition :: String,

        entityModuleMappedQueryDefinition :: String
    }
    

data EntityFieldDefinition = 
    EntityFieldDefinition
    {
      entityFieldName :: String,
      entityFieldTypeString :: String
    }
  


data ForeignKeyLinksModuleDefinition =
    
    ForeignKeyLinksModuleDefinition
    {
        fksModuleQName :: String,
        
        fksModuleFixedImports :: [String],
        
        fksModuleEntityImports :: (Set String),

        fksModuleEntityMappedQueries :: (Set String),
        
        fksModuleLinkDefs :: [String],
        
        fksModuleDynamicLinkDefs :: [String]
    }
    




