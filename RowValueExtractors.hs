{-# OPTIONS_GHC -fbang-patterns #-}

-- Copyright (c) 2008 Stephen C. Harris.
-- See COPYING file at the root of this distribution for copyright information.


module HMQ.RowValueExtractors where

import Database.HDBC

import HMQ.Query



type Row = [SqlValue]

-- A row value extractor for a single table.
type EntityRowValueExtractor a = TableAlias -> Query -> Row -> Maybe a

-- A row value extractor for a mapped query.
type QueryRowValueExtractor t = Query -> Row -> Maybe t
