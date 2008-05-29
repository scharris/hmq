-- Copyright (c) 2008 Stephen C. Harris.
-- See COPYING file at the root of this distribution for copyright information.

module HMQ.Utils.Lists where

import Data.List
import Data.Char

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy pred (x:xs) =
    let
        (xs_leading_not_ps, rest) = break pred xs
    in
      (x : xs_leading_not_ps) : splitBy pred rest
