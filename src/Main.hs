module Main where

{--
TODO
find files with similar caches
ask whick one should be left (no ncurses, just getLine or something)
delete others
--}

import System.IO
import Data.Digest.Murmur32
import System.Hclip
import qualified Data.ByteString as B
import System.Environment
import Data.Maybe
import Data.List
import Control.Exception ( catch
                         , SomeException
                         )
import System.Directory
import System.Console.ANSI


main :: IO ()
main = do
    args <- getArgs
    foo <- sequence $ map makeFileHash args
    let hashes = rollFilter . roll . hSort . clear $ foo
    sequence_ $ map cleaner hashes

cleaner :: ( Hash32, [ FilePath ] ) -> IO ()
cleaner (h, fs) = do
    putStrLn "\nFound files with same hashes:"
    addNumber fs
    setSGR [ Reset, SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green ]
    putStr "Only one will left. Which? "
    setSGR [ Reset ]
    hFlush stdout
    x <- getLine
    d <- catch ( return $ Just $ fs !! ( (read $ head $ lines x ) - 1 ))
               ( \e -> print ( e :: SomeException ) >> return Nothing )
    killMan fs d

killMan :: [ FilePath ] -> Maybe FilePath -> IO ()
killMan fs Nothing = putStrLn "Some error occured. Sparing innocent."
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

hSort :: (Ord a) => [ (a,b) ] -> [ (a,b) ]
hSort = sortBy foo
    where foo a b = compare (fst a) (fst b)

clear :: [ Maybe a ] -> [ a ]
clear = map (fromMaybe (error "FUBAR")) . filter (isJust)

makeFileHash :: FilePath -> IO ( Maybe ( Hash32, FilePath ))
makeFileHash f = catch ( makeHash f >>= ( return . Just ) )
                       ( \e -> print (e::SomeException) >> return Nothing )

makeHash :: FilePath -> IO ( Hash32, FilePath )
makeHash f = do
    h <- B.readFile f
    return ( hash32 h, f )

type Foo = [( Hash32, String )]
type Bar = [( Hash32 , [String] )]

rollFilter :: Bar -> Bar
rollFilter = filter rollPredicate
    where rollPredicate ( _ , a ) = length a > 1

roll :: Foo -> Bar
roll x = roll' x []

roll' :: Foo -> Bar -> Bar
roll' [] ys = ys
roll' ( (k,v) : xs ) [] = roll' xs [ (k,[v]) ]
roll' ( (k,v) : xs ) y@( (k0,vs) : ys )
    | k == k0   = roll' xs ( (k0,v:vs) : ys )
    | otherwise = roll' xs ( (k,[v]) : y )

