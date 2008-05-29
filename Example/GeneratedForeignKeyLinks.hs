module HMQ.Example.GeneratedForeignKeyLinks where

import Data.Dynamic

import HMQ.ForeignKeyLink

import qualified HMQ.Example.GeneratedEntities.Accounts
import qualified HMQ.Example.GeneratedEntities.Categories
import qualified HMQ.Example.GeneratedEntities.Customers
import qualified HMQ.Example.GeneratedEntities.Discs
import qualified HMQ.Example.GeneratedEntities.Employees
import qualified HMQ.Example.GeneratedEntities.Movies
import qualified HMQ.Example.GeneratedEntities.Rentals
import qualified HMQ.Example.GeneratedEntities.Stores



 -- Entity module mapped queries
accounts = HMQ.Example.GeneratedEntities.Accounts.mappedQuery

categories = HMQ.Example.GeneratedEntities.Categories.mappedQuery

customers = HMQ.Example.GeneratedEntities.Customers.mappedQuery

discs = HMQ.Example.GeneratedEntities.Discs.mappedQuery

employees = HMQ.Example.GeneratedEntities.Employees.mappedQuery

movies = HMQ.Example.GeneratedEntities.Movies.mappedQuery

rentals = HMQ.Example.GeneratedEntities.Rentals.mappedQuery

stores = HMQ.Example.GeneratedEntities.Stores.mappedQuery




customers'account_id =
    ManyToOne
        (ForeignKey
            HMQ.Example.GeneratedEntities.Customers.tableMetadata
            HMQ.Example.GeneratedEntities.Accounts.tableMetadata
            [("account_id","account_id")]
            HMQ.Example.GeneratedEntities.Customers.rowValueExtractor
            HMQ.Example.GeneratedEntities.Accounts.rowValueExtractor
        )


accounts''customers'account_id = reverseLink customers'account_id


employees'store_id =
    ManyToOne
        (ForeignKey
            HMQ.Example.GeneratedEntities.Employees.tableMetadata
            HMQ.Example.GeneratedEntities.Stores.tableMetadata
            [("store_id","store_id")]
            HMQ.Example.GeneratedEntities.Employees.rowValueExtractor
            HMQ.Example.GeneratedEntities.Stores.rowValueExtractor
        )


stores''employees'store_id = reverseLink employees'store_id


employees'manager_id =
    ManyToOne
        (ForeignKey
            HMQ.Example.GeneratedEntities.Employees.tableMetadata
            HMQ.Example.GeneratedEntities.Employees.tableMetadata
            [("manager_id","employee_id")]
            HMQ.Example.GeneratedEntities.Employees.rowValueExtractor
            HMQ.Example.GeneratedEntities.Employees.rowValueExtractor
        )


employees''employees'employee_id = reverseLink employees'manager_id


rentals'store_id =
    ManyToOne
        (ForeignKey
            HMQ.Example.GeneratedEntities.Rentals.tableMetadata
            HMQ.Example.GeneratedEntities.Stores.tableMetadata
            [("store_id","store_id")]
            HMQ.Example.GeneratedEntities.Rentals.rowValueExtractor
            HMQ.Example.GeneratedEntities.Stores.rowValueExtractor
        )


stores''rentals'store_id = reverseLink rentals'store_id


rentals'employee_id =
    ManyToOne
        (ForeignKey
            HMQ.Example.GeneratedEntities.Rentals.tableMetadata
            HMQ.Example.GeneratedEntities.Employees.tableMetadata
            [("employee_id","employee_id")]
            HMQ.Example.GeneratedEntities.Rentals.rowValueExtractor
            HMQ.Example.GeneratedEntities.Employees.rowValueExtractor
        )


employees''rentals'employee_id = reverseLink rentals'employee_id


rentals'customer_id =
    ManyToOne
        (ForeignKey
            HMQ.Example.GeneratedEntities.Rentals.tableMetadata
            HMQ.Example.GeneratedEntities.Customers.tableMetadata
            [("customer_id","customer_id")]
            HMQ.Example.GeneratedEntities.Rentals.rowValueExtractor
            HMQ.Example.GeneratedEntities.Customers.rowValueExtractor
        )


customers''rentals'customer_id = reverseLink rentals'customer_id


rentals'disc_id =
    ManyToOne
        (ForeignKey
            HMQ.Example.GeneratedEntities.Rentals.tableMetadata
            HMQ.Example.GeneratedEntities.Discs.tableMetadata
            [("disc_id","disc_id")]
            HMQ.Example.GeneratedEntities.Rentals.rowValueExtractor
            HMQ.Example.GeneratedEntities.Discs.rowValueExtractor
        )


discs''rentals'disc_id = reverseLink rentals'disc_id


discs'movie_id =
    ManyToOne
        (ForeignKey
            HMQ.Example.GeneratedEntities.Discs.tableMetadata
            HMQ.Example.GeneratedEntities.Movies.tableMetadata
            [("movie_id","movie_id")]
            HMQ.Example.GeneratedEntities.Discs.rowValueExtractor
            HMQ.Example.GeneratedEntities.Movies.rowValueExtractor
        )


movies''discs'movie_id = reverseLink discs'movie_id


movies'category_id =
    ManyToOne
        (ForeignKey
            HMQ.Example.GeneratedEntities.Movies.tableMetadata
            HMQ.Example.GeneratedEntities.Categories.tableMetadata
            [("category_id","category_id")]
            HMQ.Example.GeneratedEntities.Movies.rowValueExtractor
            HMQ.Example.GeneratedEntities.Categories.rowValueExtractor
        )


categories''movies'category_id = reverseLink movies'category_id



{- Dynamic representation of all foreign key links, both many-one and one-many.
dynamicForeignKeyLinks :: [ForeignKeyLink Dynamic Dynamic]
dynamicForeignKeyLinks =
    [
        toDynamicForeignKeyLink customers'account_id,
        toDynamicForeignKeyLink accounts''customers'account_id,
        toDynamicForeignKeyLink employees'store_id,
        toDynamicForeignKeyLink stores''employees'store_id,
        toDynamicForeignKeyLink employees'manager_id,
        toDynamicForeignKeyLink employees''employees'employee_id,
        toDynamicForeignKeyLink rentals'store_id,
        toDynamicForeignKeyLink stores''rentals'store_id,
        toDynamicForeignKeyLink rentals'employee_id,
        toDynamicForeignKeyLink employees''rentals'employee_id,
        toDynamicForeignKeyLink rentals'customer_id,
        toDynamicForeignKeyLink customers''rentals'customer_id,
        toDynamicForeignKeyLink rentals'disc_id,
        toDynamicForeignKeyLink discs''rentals'disc_id,
        toDynamicForeignKeyLink discs'movie_id,
        toDynamicForeignKeyLink movies''discs'movie_id,
        toDynamicForeignKeyLink movies'category_id,
        toDynamicForeignKeyLink categories''movies'category_id
    ]
-}
