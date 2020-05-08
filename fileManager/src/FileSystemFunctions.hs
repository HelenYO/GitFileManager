{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module FileSystemFunctions
  ( FileSystemFunctions.init
  , Dir(..)
  , FileSystem(..)
  , lsFunc
  , FS(..)
  , cdFunc
  , findFunc
  , catFunc
  ) where

import           Types

import           Control.Monad

import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import qualified Data.HashMap              as HM
import           Data.List                 (intercalate)
import           Data.List.Split
import           System.Directory
import           System.FilePath
import           System.FilePath.Find
import           System.FilePath.Posix

type FS a = StateT FileSystem IO a

--type FS = StateT FileSystem (InputT IO())
takeFName :: FilePath -> FilePath
takeFName = snd . splitFileName

initFileFromReal :: FilePath -> FS File
initFileFromReal fp = do
  let s = takeFName fp
  m <- liftIO (readFile fp)
  return (File s fp m)

initDirFromReal :: FilePath -> FS Dir
initDirFromReal fp = return (FileSystemFunctions.Dir [] [] fp)

toKVDir :: Dir -> FS (FilePath, Dir)
toKVDir d@FileSystemFunctions.Dir {path = p} = return (p, d)

toKVFile :: File -> FS (FilePath, File)
toKVFile f@File {..} = return (fPath, f)

getDirectoryContentsPathMy :: FilePath -> IO [FilePath]
getDirectoryContentsPathMy path = map (path </>) . filter (`notElem` [".", ".."]) <$> getDirectoryContents path

fileDirContentsMy :: FilePath -> FS ([FilePath], [FilePath])
fileDirContentsMy path -- будем считать, что это запускается от мапы, в которой уже лежт эта директория
 = do
  contents <- liftIO $ getDirectoryContentsPathMy path
  files <- liftIO $ filterM doesFileExist contents
  dirs <- liftIO $ filterM doesDirectoryExist contents
  return (files, dirs)

findAllFilesMy :: FilePath -> FS [FilePath]
findAllFilesMy path = do
  fs@FileSystemFunctions.FileSystem {..} <- get
  (files, dirs) <- fileDirContentsMy path
  mydirs <- mapM initDirFromReal dirs
  myfiles <- mapM initFileFromReal files
    --let newMapDir = foldr (\d acc -> HM.insert d (Types.Dir [] [] d) acc) HM.empty mydirs
  mydirsKV <- mapM toKVDir mydirs
  myfilesKV <- mapM toKVFile myfiles
  let dirstemp = HM.fromList mydirsKV
  let filestemp = HM.fromList myfilesKV
  let newMapDir = HM.unions [dirstemp, mapDir]
  let newMapFile = HM.unions [filestemp, mapFile]
  let newDir = Dir dirs files path
  let newMapDirt = HM.delete path newMapDir
  let newMapDir = HM.insert path newDir newMapDirt
  let newFs = FileSystem nowDir initDir newMapDir newMapFile
  put newFs
  nestedFiles <- mapM findAllFilesMy dirs
  return (files ++ concat nestedFiles)

init :: String -> FS String
init dir = do
  fs@FileSystem {..} <- get
  let d = FileSystemFunctions.Dir [] [] dir
  let newMapDir = HM.insert dir d mapDir
  let newFs = FileSystem dir dir newMapDir mapFile
  put newFs
  findAllFilesMy dir
  return ""

data FileSystem =
  FileSystem
    { nowDir  :: String
    , initDir :: String
    , mapDir  :: HM.Map String Dir
    , mapFile :: HM.Map String File
    }
  deriving (Show)

getNowDir :: FileSystem -> String
getNowDir FileSystem {..} = nowDir

getInitDir :: FileSystem -> String
getInitDir FileSystem {..} = initDir

getMapDir :: FileSystem -> HM.Map String Dir
getMapDir FileSystem {..} = mapDir

data Dir =
  Dir
    { folders :: [String]
    , files   :: [String]
    , path    :: String
    }
  deriving (Show)

getFolders :: Dir -> [String]
getFolders Dir {..} = folders

getFiles :: Dir -> [String]
getFiles Dir {..} = files

getPath :: Dir -> String
getPath Dir {..} = path

data File =
  File
    { name    :: String
    , fPath   :: String
    , content :: String
    }
  deriving (Show)

getName :: File -> String
getName File {..} = name

getFPath :: File -> String
getFPath File {..} = fPath

getContent :: File -> String
getContent File {..} = content

-------------------------------------------------------------------
lsFunc :: FS String
lsFunc = do
  fs@FileSystem {..} <- get
  let d = HM.lookup nowDir mapDir
  case d of
    Just args -> do
      let a = getFiles args
      let st = intercalate " " a
      let b = getFolders args
      let s = intercalate " " b
      return (st ++ " " ++ s)
    Nothing -> return ""

cdFunc :: CdOptions -> FS String
cdFunc CdOptions {..} = do
  fs@FileSystem {..} <- get
  let listOfFolders = splitOn "/" cdDir
  cdMinDir nowDir listOfFolders >>= \case
    Nothing -> return "error: can't go to this directory"
    Just args -> do
      let newNowDir = args
      let newFs = FileSystem newNowDir initDir mapDir mapFile
      put newFs
      return ("changed from " ++ nowDir ++ " to " ++ newNowDir)

cdMinDir :: String -> [String] -> FS (Maybe String)
cdMinDir now [] = return $ Just now
cdMinDir now [one] = tryCdMinDir now one
cdMinDir now (one:many) =
  tryCdMinDir now one >>= \case
    Nothing -> return Nothing
    Just next -> cdMinDir next many

tryCdMinDir :: String -> String -> FS (Maybe String)
tryCdMinDir now one = do
  let maybePath = now ++ "/" ++ one
  fs@FileSystem {..} <- get
  if one == "."
    then return (Just now)
    else if one == ".."
           then if now == initDir
                  then return Nothing
                  else do
                    let listOfFolders = splitOn "/" now
                    let s = intercalate "/" (firstParts listOfFolders)
                    return (Just s)
           else do
             let d = HM.lookup maybePath mapDir
             case d of
               Just args -> return $ Just maybePath
               Nothing -> return Nothing

firstParts :: [String] -> [String]
firstParts [x] = []
firstParts xs  = Prelude.init xs

convertMaybe :: Maybe a -> IO (Maybe a)
convertMaybe = return

findFunc :: FindOptions -> FS String
findFunc FindOptions {findName = name} = do
  fs@FileSystem {..} <- get
  t <- findAllFiles nowDir
  m <- liftIO $ filterM (\x -> convertBool (name == takeFName x)) t
  return (intercalate " " m)

convertBool :: Bool -> IO Bool
convertBool = return

fileDirContents :: String -> FS ([String], [String])
fileDirContents path = do
  fs@FileSystem {..} <- get
  let d = HM.lookup path mapDir
  case d of
    Nothing   -> return ([], [])
    Just args -> return (getFiles args, getFolders args)

findAllFiles :: String -> FS [String]
findAllFiles path = do
  (files, dirs) <- fileDirContents path
  nestedFiles <- mapM findAllFiles dirs
  return (files ++ concat nestedFiles)

catFunc :: CatOptions -> FS String
catFunc CatOptions {..} = do
  fs@FileSystem {..} <- get
  let d = HM.lookup nowDir mapDir
  case d of
    Nothing -> return ""
    Just args -> do
      let f = getFiles args
      m <- liftIO $ filterM (\x -> convertBool (catName == takeMyFileName x)) f
      if null m
        then return ""
        else do
          let f = HM.lookup (last m) mapFile
          case f of
            Nothing -> return ""
            Just args -> return (getContent args)


takeMyFileName :: String -> String
takeMyFileName s = last (splitOn "/" s)