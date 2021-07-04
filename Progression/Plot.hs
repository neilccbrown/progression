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

-- | A helper module for plotting.
module Progression.Plot (plotMulti) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Arrow ((***), (&&&))
import Control.Monad (ap, forM, liftM, when)
import Data.List (findIndex, intercalate)
import qualified Data.Map as Map
import Database.TxtSushi.FlatFile (csvFormat, formatTable, parseTable)
import System.Cmd (rawSystem)
import System.Exit (ExitCode(..))
import System.FilePath (dropExtension, takeExtension, (<.>))
import System.IO (hPutStrLn, stderr)

import Progression.Config
import Progression.Files

-- | Plots to the given destination file (using its extension as the terminal type),
-- from the given CSV file, using the given list as labels.
plotFile :: GraphSettings Definite -> (([String], [String]), FilePath) -> IO ()
plotFile settings ((rowNames, colNames), csvFile) = check =<< rawSystem "gnuplot" ("-e" : [concat cmd])
 where
   cmd =
    ["set terminal " ++ terminalType ++ " size " ++ sizeX ++ "," ++ sizeY ++ ";"
    ,"set output '" ++ get graphFilename ++ "';"
    ,"set xtics rotate;"
    ,"set xrange [-" ++ show (makeOffset 1) ++ ":"
       ++ show (fromIntegral (length rowNames - 1) + makeOffset (toInteger $ length colNames)) ++ "];"
    ,"set bmargin " ++ show ((maximum (map length rowNames) * 2) `div` 3) ++ ";"
    ,if get graphLogY then "set logscale y;" else ""
    ,"set datafile separator ',';"
    ,"set style data " ++ style ++ ";" ++ otherStyle
    ,"plot " ++ intercalate ","
       [let indices = map show [i*3 + 2, i*3 + 3, i*3 + 4]
            indicesAndExtra = case get graphType of
              GraphTypeLines -> indices
              GraphTypeBars -> indices ++ ["(" ++ show (makeOffset 1) ++ ")"]
        in "'" ++  csvFile ++ "' using ($0+" ++ show (makeOffset i) ++ "):" ++ intercalate ":" indicesAndExtra ++ ":xtic(1) title '" ++ n ++ "'"
       | (i, n) <- zip [0..] colNames]
    ]

   terminalType = case takeExtension $ get graphFilename of
     "" -> "png"
     (_:ext) -> ext

   check ExitSuccess = do hPutStrLn stderr "Executed gnuplot commands: "
                          mapM_ (hPutStrLn stderr . ("  " ++)) cmd
   check (ExitFailure _) = hPutStrLn stderr "Error executing gnuplot; have you got gnuplot installed on your system and in your path?"

   makeOffset :: Integer -> Double
   makeOffset i = (fromInteger i :: Double) / (max 8 (fromIntegral $ length colNames + 1))

   (sizeX, sizeY) = show *** show $ get graphSize

   get f = definite (f settings)

   style = case get graphType of
     GraphTypeLines -> "errorlines"
     GraphTypeBars -> "boxerrorbars"
   otherStyle = case get graphType of
     GraphTypeLines -> ""
     GraphTypeBars -> "set style fill pattern;"

-- | Plots to the given destination file (using its extension as the terminal type),
-- the given list of labels in the settings.  The first parameter is the one passed
-- to the 'graphData' function (the most recent benchmark).
plotMulti :: String -> GraphSettings Definite -> IO ()
plotMulti orig settings
  = do rowColumns <- joinMulti (get graphGroup $ orig) csvFile (map (id &&& makeFileName) $ get graphCompareTo)
       when (uncurry (&&) . ((not . null) *** (not . null)) $ rowColumns) $
         plotFile settings (rowColumns, csvFile)
  where
    csvFile = dropExtension (get graphFilename) <.> "csv"
    get f = definite (f settings)

-- I know this is really Either String; long story
data FailM a = Fail String | Fine a

instance Monad FailM where
  fail = Fail
  return = Fine
  (Fail s) >>= _ = Fail s
  (Fine x) >>= f = f x

instance Functor FailM where
  fmap = liftM

instance Applicative FailM where
  pure = return
  (<*>) = ap

-- | Joins all the result files in the list into the given destination file ready
-- to be fed to plotFile.  If the list is empty, nothing is done.
--
-- It returns lists of row and column labels for the resulting file
joinMulti :: (Map.Map String (Map.Map String BoundedMean)
                 -> GraphData)
          -> FilePath -> [(String, FilePath)] -> IO ([String], [String])
joinMulti _ _ [] = return ([], [])
joinMulti groupFunc dest allFiles
  = do allData <- sequence [parseTable csvFormat <$> readFile path | (_, path) <- allFiles]
       case mapM tableToMap allData of
         Fail err -> hPutStrLn stderr err >> return ([], [])
         -- ms must be non-empty, because "allFiles" was non-empty:
         Fine ms -> let gd = groupFunc $ Map.fromList $ zip (map fst allFiles) ms
                    in do writeFile dest $ formatTable csvFormat (mapToTable gd)
                          return ((map groupName . groupLabels) &&& (map subGroupName . subGroupLabels) $ gd)
  where
    headTail :: [a] -> FailM (a, [a])
    headTail [] = Fail "Empty file"
    headTail (x:xs) = return (x, xs)

    find' :: String -> [String] -> FailM Int
    find' s ss = case findIndex (== s) ss of
      Nothing -> Fail $ "Could not find row titled: " ++ s
      Just i -> return i

    (!) :: [a] -> Int -> FailM a
    (!) xs n | n >= length xs = Fail "Missing data in file"
             | otherwise = return $ xs !! n
    
    tableToMap :: [[String]] -> FailM (Map.Map String BoundedMean)
    tableToMap tbl = do (header, body) <- headTail tbl
                        nameIndex <- find' "Name" header
                        meanIndex <- find' "Mean" header
                        meanLBIndex <- find' "MeanLB" header
                        meanUBIndex <- find' "MeanUB" header
                        Map.fromList <$> forM body (\r ->
                          (,) <$> (r ! nameIndex) <*>
                             (BoundedMean <$>
                               (read <$> r ! meanLBIndex) <*> (read <$> r ! meanIndex) <*> (read <$> r ! meanUBIndex))
                          )

    -- No header at the moment:
    mapToTable :: GraphData -> [[String]]
    mapToTable gd = [ groupName x : concatMap (meanToStr . graphData gd x) (subGroupLabels gd)
                    | x <- groupLabels gd]
      where
        meanToStr :: BoundedMean -> [String]
        meanToStr (BoundedMean lb m ub) = map show [m, lb, ub]
