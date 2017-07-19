{-# LANGUAGE MultiWayIf #-}
module Main where

import System.IO
import System.Environment ( getArgs )
import System.Directory
  ( doesFileExist
  , listDirectory
  , withCurrentDirectory
  , doesDirectoryExist
  , makeAbsolute
  )
import Control.Monad ( filterM )
import Data.ByteString ( ByteString )
import System.Console.ANSI
import Text.Read ( readMaybe )

import Core

main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> putStrLn "Please specify one directory or list of files"
      [a] -> do
        absolute <- makeAbsolute a
        que <- doesDirectoryExist absolute
        if que
        then withCurrentDirectory absolute ( listDirectory absolute >>= work )
        else putStrLn "Giving me one file makes no sense. Give me either directory or list of files."
      _ -> work args

work :: [ FilePath ] -> IO ()
work allfiles = do
  files <- filterM doesFileExist allfiles
  let a = length allfiles - length files
  putStrLn $ "Analyzing " ++ show ( length files ) ++ " files" ++
    if | a == 1    -> " (1 directory ignored)"
       | a == 0    -> ""
       | otherwise -> " (" ++ show ( length allfiles - length files ) ++ " directories ignored)"
  hashes <- rollFilter <$> rollHashes files
  sequence_ $ map cleaner hashes

cleaner :: ( ByteString, [ FilePath ] ) -> IO ()
cleaner (h, fs) = do
  putStrLn "\nFound some doppelgangers (files with same hash):"
  addNumber fs
  setSGR [ Reset, SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green ]
  putStr "Only one will remain. Which? "
  setSGR [ Reset ]
  hFlush stdout
  x <- getLine
  killMan fs ( que x fs )
  where
  que :: String -> [ String ] -> Maybe String
  que x y = case readMaybe x :: Maybe Int of
    Just n -> if length y >= n then Just ( y !! (n-1)) else Nothing
    Nothing -> Nothing

addNumber :: [ String ] -> IO ()
addNumber s = sequence_ $ map foo $ zip s [1..]
  where
  foo (a,b) = do
    putStr $ show b ++ "  "
    setSGR [ Reset, SetConsoleIntensity BoldIntensity ]
    putStrLn a
    setSGR [ Reset ]
