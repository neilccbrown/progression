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

-- | A module containing helper functions for the interactive prompts.
module Progression.Prompt (promptOne, promptManyComma, splitOnCommas) where

import Control.Applicative ((<$>))
import Data.List (isPrefixOf, unfoldr)
import Data.Maybe (fromMaybe)
import System.Console.Haskeline (getInputLine, runInputT, Settings(Settings))
import System.Console.Haskeline.Completion (completeWord, simpleCompletion)
import Text.Show (showListWith)

-- | Prompts for a single item, using the given message, followed by some of the
-- list of suggestions.  All suggestions are used to form the tab completion.
-- The result will be trimmed of leading and trailing spaces.
promptOne :: String -> [String] -> IO String
promptOne msg opts = trim . fromMaybe "" <$> runInputT settings (getInputLine (msg ++ showListWith (++) fewOpts "" ++ ": "))
  where
    settings = Settings complete Nothing True
    complete = completeWord Nothing "," (return . completeOpt)
    completeOpt str = [simpleCompletion opt | opt <- opts, str `isPrefixOf` opt]

    fewOpts
      | length opts <= 5 = opts
      | otherwise = take 5 opts ++ ["..."]

-- | Prompts for one or more comma-separated items, using the given message and
-- suggestions (some of which will be shown with the message).  The results will
-- each be trimmed of leading and trailing spaces, and the whole list will have
-- empty items removed.
promptManyComma :: String -> [String] -> IO [String]
promptManyComma = ((<$>) (filter (not . null) . map trim . splitOnCommas) .) . promptOne

splitOnCommas :: String -> [String]
splitOnCommas = unfoldr splitFirstComma
  where
    splitFirstComma :: String -> Maybe (String, String)
    splitFirstComma [] = Nothing
    splitFirstComma s = case span (/= ',') s of
      (_, []) -> Just (s, "")
      (pre, _:post) -> Just (pre, post)

-- | Trims leading and trailing spaces.  Probably very inefficient, but it shouldn't
-- matter for our application.
trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
