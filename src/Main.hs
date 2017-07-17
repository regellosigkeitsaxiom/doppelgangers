{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
module Main where

import System.IO
import Crypto.Hash.SHA256 as H
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import System.Environment ( getArgs )
import Data.List ( delete )
import Control.Exception ( catch , SomeException)
import System.Directory ( removeFile, doesFileExist, listDirectory, withCurrentDirectory )
import System.Console.ANSI
import Text.Read ( readMaybe )
import Control.Monad ( foldM, filterM )


main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> putStrLn "Please specify one directory or list of files"
      [a] -> withCurrentDirectory a ( listDirectory a >>= work )
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

cleaner :: ( B.ByteString, [ FilePath ] ) -> IO ()
cleaner (h, fs) = do
    putStrLn "\nFound some doppelgangers (files with same hash):"
    addNumber fs
    setSGR [ Reset, SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green ]
    putStr "Only one will remain. Which? "
    setSGR [ Reset ]
    hFlush stdout
    x <- getLine
    killMan fs ( que x fs )

que :: String -> [ String ] -> Maybe String
que x y = case readMaybe x :: Maybe Int of
  Just n -> if length y >= n then Just ( y !! (n-1)) else Nothing
  Nothing -> Nothing

killMan :: [ FilePath ] -> Maybe FilePath -> IO ()
killMan fs Nothing = putStrLn "Some error occured. Sparing innocents."
killMan fs (Just f) = deleteRogues fs f >> ( putStrLn $ f ++ " survived. Others not.")

deleteRogues :: [ FilePath ] -> FilePath -> IO ()
deleteRogues fs f = do
    let hangmans = delete f fs
    sequence_ $ map removeFile hangmans

addNumber :: [ String ] -> IO ()
addNumber s = sequence_ $ map foo $ zip s [1..]
    where foo (a,b) = do
            putStr $ show b ++ "  "
            setSGR [ Reset, SetConsoleIntensity BoldIntensity ]
            putStrLn a
            setSGR [ Reset ]

makeHash :: FilePath -> IO ( Maybe ( B.ByteString, FilePath ))
makeHash f = catch ( makeHash_ f >>= ( return . Just ) )
                          ( \e -> print (e::SomeException) >> return Nothing )
  where
  makeHash_ :: FilePath -> IO ( B.ByteString, FilePath )
  makeHash_ f = do
    handle <- openFile f ReadMode
    hash <- H.finalize <$> loop handle H.init
    return ( hash, f )
  loop handle !ctx = do
    bytes <- B.hGet handle (2^20)
    if C8.null bytes
    then hClose handle >> return ctx
    else loop handle $ update ctx bytes

type Bar = [( B.ByteString , [String] )]

rollFilter :: Bar -> Bar
rollFilter = filter rollPredicate
    where rollPredicate ( _ , a ) = length a > 1

rollHashes :: [ FilePath ] -> IO [( B.ByteString, [ String ] )]
rollHashes files = foldM rolly [] files
  where
  rolly :: Bar -> FilePath -> IO Bar
  rolly !accum file = do
    single <- makeHash file
    case single of
      Nothing -> return accum
      Just hash -> return $ appendHash accum hash

appendHash :: Bar -> ( B.ByteString, String ) -> Bar
appendHash [] (kk,vv) = [ (kk,[vv])]
appendHash ((k,v):xs) new@(kk,vv)
  | k == kk = (k,vv:v):xs
  | otherwise = (k,v) : appendHash xs new
