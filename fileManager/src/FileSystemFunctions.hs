{-# LANGUAGE RecordWildCards #-}

module FileSystemFunctions
  ( FileSystemFunctions.init
  , Dir(..)
  , FileSystem(..)
  , lsFunc
  , FS(..)
  ) where

import           Control.Monad

import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import qualified Data.HashMap              as HM
import           Data.List                 (intercalate)
import           System.Directory
import           System.FilePath
import           System.FilePath.Find
import           System.FilePath.Posix

type FS a = StateT FileSystem IO a

--type FS = StateT FileSystem (InputT IO())
takeFName :: FilePath -> FilePath
takeFName = snd . splitFileName

--  initFileFromReal :: FilePath -> FS File
--  initFileFromReal fp = do
--    let s = takeFName fp
--    return (File s fp) -- будем надеяться, что пути одинаковые всегда
initDirFromReal :: FilePath -> FS Dir
initDirFromReal fp = return (FileSystemFunctions.Dir [] [] fp)

toKV :: Dir -> FS (FilePath, Dir)
toKV d@FileSystemFunctions.Dir {path = p} = return (p, d)

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
    --let newMapDir = foldr (\d acc -> HM.insert d (Types.Dir [] [] d) acc) HM.empty mydirs
  mydirsKV <- mapM toKV mydirs
  let dirstemp = HM.fromList mydirsKV
  let newMapDir = HM.unions [dirstemp, mapDir]
  let newDir = Dir dirs files path
  let newMapDirt = HM.delete path newMapDir
  let newMapDir = HM.insert path newDir newMapDirt
  let newFs = FileSystem nowDir initDir newMapDir
  put newFs
  nestedFiles <- mapM findAllFilesMy dirs
  return (files ++ concat nestedFiles)

init :: String -> FS String
init dir = do
  fs@FileSystem {..} <- get
  let d = FileSystemFunctions.Dir [] [] dir
  let newMapDir = HM.insert dir d mapDir
  let newFs = FileSystem dir dir newMapDir
  put newFs
  findAllFilesMy dir
  return ""

data FileSystem =
  FileSystem
    { nowDir  :: String
    , initDir :: String
    , mapDir  :: HM.Map String Dir
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
    { name  :: String
    , fPath :: String
    }
  deriving (Show)

getName :: File -> String
getName File {..} = name

getFPath :: File -> String
getFPath File {..} = fPath

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

convertMaybe :: Maybe a -> IO (Maybe a)
convertMaybe = return
