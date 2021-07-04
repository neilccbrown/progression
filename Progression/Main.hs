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

-- | The primary module in Progression; contains methods that you can use as the
-- main method of your wrapper program.  Typically, to use Progression, you create
-- a Haskell program that defines/imports the benchmarks, and passes them to the
-- 'defaultMain' method below.  You then compile that program and run it to record
-- and graph your benchmarks.
module Progression.Main (defaultMain, defaultMainWith) where

import qualified Criterion.Config as CriterionConfig (Config(..), defaultConfig)
import qualified Criterion.Main as Criterion (defaultMainWith)
import Criterion.Types (Benchmark(..))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid (Last(..), mempty)
import qualified Data.Set as Set
import System.Environment (getArgs, withArgs)

import Progression.Config
import Progression.Files
import Progression.Plot
import Progression.Prompt

allPrefixes :: Benchmark -> Set.Set String
allPrefixes (Benchmark _label _bench) = Set.empty
allPrefixes (BenchCompare     _bench) = Set.empty
allPrefixes (BenchGroup prefix benches)
  = Set.mapMonotonic (prefix ++) $
      foldl Set.union (Set.singleton "") $
        map (Set.mapMonotonic ('/' :) . allPrefixes) benches

-- | Like 'defaultMain' but you can specify the default configuration.  Command-line
-- argument processing is still performed, and command-line settings will take
-- precedence over the config passed in.
defaultMainWith :: Config -> Benchmark -> IO ()
defaultMainWith defaultConfig bench = do
  origArgs <- getArgs
  let (ourArgs, criterionArgs) = span (/= "--") origArgs
  cfg <- processArgs defaultConfig ourArgs
  mainWith cfg (dropWhile (== "--") criterionArgs) bench

-- | Takes the given benchmark (which is likely a benchmark group) and runs it
-- as part of Progression, recording the results and producing graphs.  The Benchmark
-- type is imported from the Criterion library, so see the documentation for Criterion
-- to find out what can be benchmarked and any issues that might arise in the benchmarking.
--
-- This function will process the command-line arguments of the program, consuming
-- any progression arguments, and passing any arguments that occur after a \"--\"
-- argument on to Criterion.  If you want to perform further argument processing,
-- it is best to do this before the call, and wrap the call in 'withArgs'.
defaultMain :: Benchmark -> IO ()
defaultMain = defaultMainWith mempty

mainWith :: Config -> [String] -> Benchmark -> IO ()
mainWith cfg criterionArgs bench = do
  compareChoices <- findResultFiles
  (name, r) <- if not running then return (Nothing, Nothing) else do
         prefixes <- if length prefixChoices <= 1 || all (== "") prefixChoices
                       then return []
                       else optL (runPrefixes . cfgRun)
                                 (promptManyComma "Run which prefixes " prefixChoices)
         name <- opt (runStoreAs . cfgRun)
                     (promptOne "Store as" compareChoices)
         return $ (,) (Just name) $ Just $ withArgs (prefixes ++ criterionArgs) $
           Criterion.defaultMainWith (CriterionConfig.defaultConfig {CriterionConfig.cfgSummaryFile = (Last $ Just (makeFileName name))})
           (return ()) 
           [bench]
  g <- if not graphing then return Nothing else do
         cmp <- opt (graphCompareTo . cfgGraph)
                    (promptManyComma "Compare to " compareChoices)
         let d = Definite
             defaultGraph = GraphSettings (d $ maybe cmp (:cmp) name)
                              (d "plot.png") (d (1024, 768)) (d False) (d GraphTypeBars) (d $ \m -> groupBench m . normalise m)
         return $ Just $ plotMulti (fromMaybe (fromMaybe (error "Compare list is empty") $ listToMaybe cmp) name) $ defaultGraph `override` cfgGraph cfg
  mrun r
  mrun g
  where
    prefixChoices :: [String]
    prefixChoices = Set.toList $ allPrefixes bench

    mrun = fromMaybe (return ())

    opt f m = maybe m return (f cfg)
    optL f m = case f cfg of
                 [] -> m
                 xs -> return xs
    graphing = fromMaybe RunAndGraph (cfgMode cfg) `elem` [RunAndGraph, JustGraph]
    running = fromMaybe RunAndGraph (cfgMode cfg) `elem` [RunAndGraph, JustRun]
