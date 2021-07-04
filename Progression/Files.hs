-- Progression.
-- Copyright (c) 2010, Neil Brown.
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
--  * Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--  * Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--  * The name of Neil Brown may not be used to endorse or promote products derived from
--    this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
-- IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
-- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
-- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
-- CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
-- EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
-- PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-- | Some helper functions for dealing with the results (CSV) files.
module Progression.Files (findResultFiles, makeFileName) where

import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Data.List (isPrefixOf, sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import System.FilePath (splitExtension, (<.>))
import System.Directory (getDirectoryContents, getModificationTime)

specialPrefix :: String
specialPrefix = "bench-"

-- | Given a label for a result-set, turns it into a CSV file name.
--
-- Currently this is done by prepending \"bench-\" and appending \".csv\".
makeFileName :: String -> FilePath
makeFileName s = (specialPrefix ++ s) <.> "csv"

-- | Sorts a list of labels (not CSV file names) by their modification time, latest
-- first.
sortByModificationTime :: [String] -> IO [String]
sortByModificationTime
  = liftM (map fst . sortBy ((descending .) . comparing snd)) . mapM addModificationTime
  where
    addModificationTime f = (,) f <$> getModificationTime (makeFileName f)

    descending LT = GT
    descending EQ = EQ
    descending GT = LT

-- | Finds all the results files in the working directory, and returns a list of
-- their labels.
findResultFiles :: IO [String]
findResultFiles
  = sortByModificationTime =<< ((mapMaybe benchPrefix . mapMaybe csvStem) <$> getDirectoryContents ".")
  where
    csvStem :: FilePath -> Maybe String
    csvStem f = case splitExtension f of
      (stem, ".csv") -> Just stem
      _ -> Nothing

    benchPrefix :: String -> Maybe String
    benchPrefix f
      | specialPrefix `isPrefixOf` f = Just $ drop (length specialPrefix) f
      | otherwise = Nothing
