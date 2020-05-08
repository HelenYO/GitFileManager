module Functions
  ( cdFunc
 -- , lsFunc
  , catFunc
  , mkFileFunc
  , mkDirFunc
  , rmFunc
  , wr2FileFunc
  , findFunc
  , dirFunc
  , infFunc
  ) where

import           Types

import           Control.Monad
--import Data.List
import           System.Directory
import           System.FilePath
import           System.FilePath.Find
import           System.FilePath.Posix






----------------------------------------------------------------------------------------

cdFunc :: CdOptions -> IO ()
cdFunc CdOptions {cdDir = dir} = do
  exist <- doesPathExist dir
  if exist
    then do
      print "changed directory from"
      d <- getCurrentDirectory
      print d
      print "to"
      setCurrentDirectory dir
      d <- getCurrentDirectory
      print d
    else print "error: this directory doesn't exist"

lsFunc :: IO ()
lsFunc = do
  files <- listDirectory "."
  print files

catFunc :: CatOptions -> IO ()
catFunc CatOptions {catName = name} = do
  exist <- doesFileExist name
  if exist
    then readFile name >>= putStrLn
    else print "error: this file doesn't exist"

mkFileFunc :: MkFileOptions -> IO ()
mkFileFunc MkFileOptions {mkfileName = name} = do
  exist <- doesFileExist name
  if exist
    then print "file is already exist"
    else writeFile name ""

mkDirFunc :: MkDirOptions -> IO ()
mkDirFunc MkDirOptions {mkDir = dir} = do
  exist <- doesPathExist dir
  if exist
    then print "directory is already exist"
    else createDirectory dir

rmFunc :: RmOptions -> IO ()
rmFunc RmOptions {rmPath = path} = do
  exist <- doesFileExist path
  if exist
    then removeFile path
    else do
      exist <- doesPathExist path
      if exist
        then removeDirectoryRecursive path
        else print "file|directory isn't found"

wr2FileFunc :: Write2FileOptions -> IO ()
wr2FileFunc Write2FileOptions {rewrite = rewrite, wrFile = name, wrData = wrData} = do
  exist <- doesFileExist name
  if exist
    then if rewrite
           then writeFile name wrData
           else appendFile name wrData
    else print "file isn't found"

dirFunc :: DirOptions -> IO ()
dirFunc DirOptions {dirName = name} = do
  t <- findAllFiles name
  print t

findFunc :: FindOptions -> IO ()
findFunc FindOptions {findName = name} = do
  t <- findAllFiles "."
  m <- filterM (\x -> convertBool (name == takeFName x)) t
  print m

infFunc :: InfOptions -> IO ()
infFunc InfOptions {infName = name} = do
  exist <- doesFileExist name
  if exist
    then print "fileInfo"
    else do
      exist <- doesPathExist name
      if exist
        then removeDirectoryRecursive name
        else print "file|directory isn't found"

--helpful
convertBool :: Bool -> IO Bool
convertBool = return

getDirectoryContentsPath :: FilePath -> IO [FilePath]
getDirectoryContentsPath path = map (path </>) . filter (`notElem` [".", ".."]) <$> getDirectoryContents path

fileDirContents :: FilePath -> IO ([FilePath], [FilePath])
fileDirContents path = do
  contents <- getDirectoryContentsPath path
  files <- filterM doesFileExist contents
  dirs <- filterM doesDirectoryExist contents
  return (files, dirs)
  
takeFName :: FilePath -> FilePath
takeFName = snd . splitFileName

findAllFiles :: FilePath -> IO [FilePath]
findAllFiles path = do
  (files, dirs) <- fileDirContents path
  nestedFiles <- mapM findAllFiles dirs
  return (files ++ concat nestedFiles)
