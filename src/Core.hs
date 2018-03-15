{-# LANGUAGE BangPatterns #-}
module Core where

import System.IO
import Crypto.Hash.SHA256 as H
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.List ( delete )
import Control.Exception ( catch , SomeException)
import System.Directory ( removeFile )
import Control.Monad ( foldM )

killMan :: [ FilePath ] -> Maybe FilePath -> IO ()
killMan fs Nothing = putStrLn "Some error occured. Sparing innocents."
killMan fs (Just f) = deleteRogues fs f >> ( putStrLn $ f ++ " survived. Others not." )

deleteRogues :: [ FilePath ] -> FilePath -> IO ()
deleteRogues fs f = do
    let hangmans = delete f fs
    mapM_ removeFile hangmans

makeHash :: FilePath -> IO ( Maybe ( B.ByteString, FilePath ))
makeHash f = catch ( Just <$> makeHash_ f )
                          ( \e -> print (e::SomeException) >> return Nothing )
  where
  makeHash_ :: FilePath -> IO ( B.ByteString, FilePath )
  makeHash_ file = do
    handle <- openFile f ReadMode
    hashed <- H.finalize <$> loop handle H.init
    return ( hashed, file )
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
rollHashes = foldM rolly []
  where
  rolly :: Bar -> FilePath -> IO Bar
  rolly !accum file = do
    single <- makeHash file
    case single of
      Nothing -> return accum
      Just hashed -> return $ appendHash accum hashed

appendHash :: Bar -> ( B.ByteString, String ) -> Bar
appendHash [] (kk,vv) = [ (kk,[vv])]
appendHash ((k,v):xs) new@(kk,vv)
  | k == kk = (k,vv:v):xs
  | otherwise = (k,v) : appendHash xs new
