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

-- | A module exposing the configuration for progression.
--
-- Each item is either a Maybe type or a list.  The values Nothing or the empty
-- list indicate a lack of preference and will be over-ridden by the other setting
-- in an mappend; settings can be joined together using their monoid instances.
module Progression.Config (BoundedMean(..), RunSettings(..), GraphSettings(..), GroupName(..),
  SubGroupName(..), GraphData(..), GraphDataMapping, GraphType(..), Mode(..), Config(..),
  Definite(..), groupBench, groupVersion, normalise, override, processArgs) where

import Control.Arrow ((&&&))
import Control.Monad ((>=>))
import Data.Char (isDigit)
import Data.List (intercalate, partition)
import qualified Data.Map as Map (Map, elems, differenceWith, intersection, keys, lookup, map, null)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo)
import System.Environment (getProgName)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)

import Progression.Prompt (splitOnCommas)

-- | A type that holds the value for a mean with bounds.
data BoundedMean = BoundedMean { meanLB :: Double, mean :: Double, meanUB :: Double }

-- | The settings for running benchmarks; which prefixes to run (empty list means
-- no preference, i.e. all -- not none) and where to put the result.
data RunSettings = RunSettings { runPrefixes :: [String], runStoreAs :: Maybe String }

-- | The identity functor
newtype Definite a = Definite { definite :: a }

-- | The name of a particular group on the x-axis; depending on your choice, this
-- could be a benchmark name or a version name.
newtype GroupName = GroupName { groupName :: String }

-- | The name of a particular element of a group (for line graphs this is the name
-- of the line; for bar charts this is a particular recurring bar colour).
newtype SubGroupName = SubGroupName { subGroupName :: String }

-- | Some data that is ready to graph.  There are the group labels (groups on the
-- x-axis) which will be plotted in the order given in the list, sub-group labels
-- (either bar colours or lines), and a function that gets the data for a given
-- group label and sub-group label.
--
-- It is expected that 'graphData' will only ever be called with combinations of
-- the labels in the attached lists, but that it should return a sensible (i.e.
-- non-bottom) value in all these cases.
data GraphData = GraphData {groupLabels :: [GroupName], subGroupLabels :: [SubGroupName], graphData :: GroupName -> SubGroupName -> BoundedMean}

-- | The type of a graph; lines or bars
data GraphType = GraphTypeLines | GraphTypeBars

-- | A function for mapping raw data (i.e. read from CSV files) into data arranged
-- for plotting.
--
-- The first parameter is the name of the version most recently recorded, or
-- (if just graphing is taking place) the name of the first version listed by the
-- user.
--
-- The second parameter is a map from version name (e.g. fused-memo) to: a map from benchmark name
-- (e.g. calculate-primes) to the recorded mean.
--
-- The return is the arranged 'GraphData'.
--
-- The default is a composition of 'groupBench' and 'normalise'.
type GraphDataMapping = String -> Map.Map String (Map.Map String BoundedMean) -> GraphData

-- | The settings for plotting graphs; which labels (besides the one created by
-- the current run, if applicable) to feature in the graph, and where to store
-- the file (plot.png, by default).
data GraphSettings m = GraphSettings { graphCompareTo :: m [String]
                                     , graphFilename :: m String
                                     , graphSize :: m (Int, Int)
                                     , graphLogY :: m Bool
                                     , graphType :: m GraphType
                                     , graphGroup :: m GraphDataMapping
                                     }

-- | The mode; just running and recording a benchmark, just graphing existing results,
-- or running a benchmark and produce a graph (the default).
data Mode = JustRun | RunAndGraph | JustGraph
  deriving Eq

-- | The mode (RunAndGraph, by default), the run settings and the graph settings.
data Config = Config {cfgMode :: Maybe Mode, cfgRun :: RunSettings, cfgGraph :: GraphSettings Maybe }

instance Monoid Config where
  mempty = Config Nothing mempty mempty
  mappend (Config m r g) (Config m' r' g') = Config (m||*m') (mappend r r') (mappend g g')

instance Monoid RunSettings where
  mempty = RunSettings mempty mempty
  mappend (RunSettings p s) (RunSettings p' s') = RunSettings (p++p') (s||*s')

instance Monoid (GraphSettings Maybe) where
  mempty = GraphSettings mempty Nothing Nothing Nothing Nothing Nothing
  mappend (GraphSettings c f sz l t srt) (GraphSettings c' f' sz' l' t' srt')
    = GraphSettings (c `mappend` c') (f ||* f') (sz ||* sz') (l ||* l') (t ||* t') (srt ||* srt')

-- Over-rides the LHS with the RHS (if non-Nothing)
override :: GraphSettings Definite -> GraphSettings Maybe -> GraphSettings Definite
override (GraphSettings c f sz l t srt) (GraphSettings c' f' sz' l' t' srt')
  = GraphSettings (c % c') (f % f') (sz % sz') (l % l') (t % t') (srt % srt')
  where
    a % b = Definite $ fromMaybe (definite a) b

(||*) :: Maybe a -> Maybe a -> Maybe a
x ||* Nothing = x
_ ||* y = y

-- Brings the specified element to the front of the list
toTop :: Eq a => a -> [a] -> [a]
toTop x = uncurry (++) . partition (== x)

-- | A function that turns versions into major groups, benchmarks into sub-groups,
-- and brings the name of the latest version to the head of the group list.
groupVersion :: GraphDataMapping
groupVersion n m
  | Map.null m = GraphData [] [] (const $ const $ BoundedMean 0 0 0)
  | otherwise = GraphData (map GroupName $ toTop n $ Map.keys m) (map SubGroupName $ Map.keys $ foldl1 Map.intersection $ Map.elems m)
                  (\(GroupName x) (SubGroupName y) -> fromMaybe (error "defaultGroup: Unknown version") $
                    Map.lookup y (fromMaybe (error "defaultGroup: Unknown benchmark") $ Map.lookup x m))

-- | A function that turns benchmarks into major groups, versions into sub-groups,
-- and brings the name of the latest version to the head of the sub-group list.
groupBench :: GraphDataMapping
groupBench n m
  | Map.null m = GraphData [] [] (const $ const $ BoundedMean 0 0 0)
  | otherwise = GraphData (map GroupName $ Map.keys $ foldl1 Map.intersection $ Map.elems m) (map SubGroupName $ toTop n $ Map.keys m) 
                  (\(GroupName x) (SubGroupName y) -> fromMaybe (error "defaultGroup: Unknown version") $
                    Map.lookup x (fromMaybe (error "defaultGroup: Unknown benchmark") $ Map.lookup y m))


-- | A function that normalises the given data (second parameter) by dividing by the time taken by
-- the given version (first parameter).  Benchmarks where the divisor is zero or
-- missing have their times left untouched.
--
-- This is intended to be applied before 'groupBench' or 'groupVersion'.
normalise :: String -> Map.Map String (Map.Map String BoundedMean) -> Map.Map String (Map.Map String BoundedMean)
normalise baseName vals = Map.map (flip (Map.differenceWith normaliseTo) standard) vals
  where
    standard = fromMaybe (error "normalise: base not found") $ Map.lookup baseName vals

    normaliseTo (BoundedMean lb m ub) (BoundedMean _ n _)
      | n == 0 = Just $ BoundedMean lb m ub
      | otherwise = Just $ BoundedMean (lb / n) (m / n) (ub / n)

data OptM a = ShowHelp | Error String | Result a

instance Monad OptM where
  fail = Error
  return = Result

  ShowHelp >>= _ = ShowHelp
  (Error e) >>= _ = Error e
  (Result x) >>= f = f x

options :: [OptDescr (Config -> OptM Config)]
options  = [Option "p" ["prefixes"] (ReqArg prefix "PREFIX")
              "Run the specified comma-separated list of prefixes (can be given multiple times)"
           ,Option "n" ["name"] (ReqArg name "NAME")
              "Store the results with the specified name"
           ,Option "c" ["compare"] (ReqArg compareTo "COMPARISON")
              "Compare the given comma-separated list of previous recordings (can be given multiple times).  Automatically includes the current recording, if any"
           ,Option [] ["plot"] (ReqArg plot "FILENAME")
              "Store the plot as the given filename.  The extension, if any, is used to set the gnuplot terminal type"
           ,Option [] ["plot-size"] (ReqArg plotSize "XxY")
              "Plot with the given size (e.g. 640x480)"
           ,Option [] ["plot-log-y"] (NoArg logY)
              "Plot with a logarithmic Y-axis"
           ,Option "t" ["plot-type"] (ReqArg plotType "TYPE")
              "Draw the plot using \"bars\" (default) or \"lines\""
           ,Option "m" ["mode"] (ReqArg mode "MODE")
              "Specify \"graph\" to just draw a graph, \"run\" to just run the benchmark, or \"normal\" (the default) to do both"
           ,Option "g" ["group"] (ReqArg groupUsing "GROUP")
              "Groups the benchmarks; \"normal-bench\" (the default) for grouping by benchmark and normalising, \"bench\", \"normal-version\", \"version\" for grouping by version"
           ,Option "h" ["help"] (NoArg help)
              "Display this help message"
           ]
  where
    add :: (Monoid monoid, Monad monad) => monoid -> monoid -> monad monoid
    add x c = return $ c `mappend` x
    prefix p = add $ mempty {cfgRun = mempty {runPrefixes = splitOnCommas p} }
    name n = add $ mempty {cfgRun = mempty { runStoreAs = Just n} }
    compareTo c = add $ mempty {cfgGraph = mempty {graphCompareTo = Just (splitOnCommas c)} }
    plot c = add $ mempty {cfgGraph = mempty {graphFilename = Just c} }
    plotSize c = do let (x, xrest) = span isDigit c
                    case xrest of
                      ('x': y) | not (null x) && not (null y) && all isDigit y ->
                        let sz = (read x, read y)
                        in add $ mempty {cfgGraph = mempty {graphSize = Just sz} }
                      _ -> const $ Error $ "Malformed size: \"" ++ c ++ "\""
    logY = add $ mempty {cfgGraph = mempty {graphLogY = Just True}}

    mode "graph" = add $ mempty {cfgMode = Just JustGraph}
    mode "run" = add $ mempty {cfgMode = Just JustRun}
    mode "normal" = add $ mempty {cfgMode = Just RunAndGraph}
    mode m = const $ Error $ "Invalid mode setting: \"" ++ m ++ "\""

    groupUsing "normal-bench" = add $ mempty {cfgGraph = mempty {graphGroup = Just $ groupBench `after` normalise } }
    groupUsing "bench" = add $ mempty {cfgGraph = mempty {graphGroup = Just $ groupBench } }
    groupUsing "normal-version" = add $ mempty {cfgGraph = mempty {graphGroup = Just $ groupVersion `after` normalise } }
    groupUsing "version" = add $ mempty {cfgGraph = mempty {graphGroup = Just $ groupVersion } }
    groupUsing g = const $ Error $ "Invalid group setting: \"" ++ g ++ "\""

    f `after` g = uncurry (.) . (f &&& g)

    plotType "bars" = add $ mempty {cfgGraph = mempty {graphType = Just GraphTypeBars} }
    plotType "lines" = add $ mempty {cfgGraph = mempty {graphType = Just GraphTypeLines} }
    plotType t = const $ Error $ "Invalid plot type setting: \"" ++ t ++ "\""

    --TODO refactor the above three functions into lookup-lists

    help = const ShowHelp

-- | Processes the given arguments (got from getArgs, typically) to adjust the
-- given default configuration, returning the resulting configuration.  Exits the
-- whole program with an error if there is a problem, or if the user specified
-- @-h@ (in which case it exits after printing the options).
processArgs :: Config -> [String] -> IO Config
processArgs defaultConfig ourArgs
  = let (cfgFuncs, nonOpt, otherErr) = getOpt Permute options ourArgs
        cfgResult = foldl (>=>) return cfgFuncs $ defaultConfig
    in case (cfgResult, not $ null $ nonOpt, not $ null $ otherErr) of
         (Error err, _, _) -> exitErr $ err ++ intercalate "," otherErr
         (_, _, True) -> exitErr $ intercalate "," otherErr
         (_, True, _) -> exitErr $ "Unrecognised options: " ++ intercalate "," nonOpt
         (ShowHelp, _, _) -> do progName <- getProgName
                                putStrLn $ usageInfo (progName ++ " [PROGRESSION-ARGS [-- CRITERION-ARGS]]") options
                                exitWith ExitSuccess
         (Result cfg, False, False) -> return cfg
  where
    exitErr e = hPutStrLn stderr e >> exitWith (ExitFailure 1)
