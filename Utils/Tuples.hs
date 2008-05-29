{-# OPTIONS_GHC -fbang-patterns #-}

-- Copyright (c) 2008 Stephen C. Harris.
-- See COPYING file at the root of this distribution for copyright information.

module HMQ.Utils.Tuples where

import Data.List


apply2 :: (a -> b) -> (a,a) -> (b,b)
apply2 f (a1,a2) = (f a1, f a2)

apply3 :: (a -> b) -> (a,a,a) -> (b,b,b)
apply3 f (a1,a2,a3) = (f a1, f a2, f a3)

apply4 :: (a -> b) -> (a,a,a,a) -> (b,b,b,b)
apply4 f (a1,a2,a3,a4) = (f a1, f a2, f a3, f a4)

apply5 :: (a -> b) -> (a,a,a,a,a) -> (b,b,b,b,b)
apply5 f (a1,a2,a3,a4,a5) = (f a1, f a2, f a3, f a4, f a5)


fst3  (a,_,_) = a
fst4  (a,_,_,_) = a
fst5  (a,_,_,_,_) = a
fst6  (a,_,_,_,_,_) = a
fst7  (a,_,_,_,_,_,_) = a
fst8  (a,_,_,_,_,_,_,_) = a
fst9  (a,_,_,_,_,_,_,_,_) = a
fst10 (a,_,_,_,_,_,_,_,_,_) = a


strict2Tuple !a1 !a2 = (a1,a2)
strict3Tuple !a1 !a2 !a3 = (a1,a2,a3)
strict4Tuple !a1 !a2 !a3 !a4 = (a1,a2,a3,a4)
strict5Tuple !a1 !a2 !a3 !a4 !a5 = (a1,a2,a3,a4,a5)
strict6Tuple !a1 !a2 !a3 !a4 !a5 !a6 = (a1,a2,a3,a4,a5,a6)
strict7Tuple !a1 !a2 !a3 !a4 !a5 !a6 !a7 = (a1,a2,a3,a4,a5,a6,a7)
strict8Tuple !a1 !a2 !a3 !a4 !a5 !a6 !a7 !a8 = (a1,a2,a3,a4,a5,a6,a7,a8)


flattenNRightPairingsSource n =
    "flatten" ++ show n ++ "RightPairings " ++ nRightPairings ++ " = " ++ mTuple
    where
      m = n + 1
      nRightPairings = replicate n '(' ++ "a1," ++ (concat $ intersperse "," $ map (\k -> "a" ++ show k ++ ")") [2..m])
      mTuple = "(" ++ (concat $ intersperse "," $ map (\k -> "a" ++ show k) [1..m]) ++ ")"


printRightPairingFlattenerDefs = mapM_ putStrLn $ map flattenNRightPairingsSource [2..10]

-- Generated via printRightPairingFlattenerDefs
flatten2RightPairings ((a1,a2),a3) = (a1,a2,a3)
flatten3RightPairings (((a1,a2),a3),a4) = (a1,a2,a3,a4)
flatten4RightPairings ((((a1,a2),a3),a4),a5) = (a1,a2,a3,a4,a5)
flatten5RightPairings (((((a1,a2),a3),a4),a5),a6) = (a1,a2,a3,a4,a5,a6)
flatten6RightPairings ((((((a1,a2),a3),a4),a5),a6),a7) = (a1,a2,a3,a4,a5,a6,a7)
flatten7RightPairings (((((((a1,a2),a3),a4),a5),a6),a7),a8) = (a1,a2,a3,a4,a5,a6,a7,a8)
flatten8RightPairings ((((((((a1,a2),a3),a4),a5),a6),a7),a8),a9) = (a1,a2,a3,a4,a5,a6,a7,a8,a9)
flatten9RightPairings (((((((((a1,a2),a3),a4),a5),a6),a7),a8),a9),a10) = (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
flatten10RightPairings ((((((((((a1,a2),a3),a4),a5),a6),a7),a8),a9),a10),a11) = (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11)
