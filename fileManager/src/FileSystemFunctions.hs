{-# LANGUAGE RecordWildCards #-}
module FileSystemFunctions
  ( FileSystemFunctions.init
  , Dir(..)
  , FileSystem(..)
  )where
  
  
  
  import           Control.Monad
  --import Functions
  --import Types
  import           Control.Monad.Trans.State
  import qualified Data.HashMap              as HM
  import           System.FilePath.Posix
  import           System.Directory
  import           System.FilePath
  import           System.FilePath.Find
  import Control.Monad.IO.Class (liftIO)
  
  type FS a = StateT FileSystem IO a
  
  takeFName :: FilePath -> FilePath
  takeFName = snd . splitFileName
  
  initFileFromReal :: FilePath -> FS File
  initFileFromReal fp = do
    let s = takeFName fp
    return (File s fp) -- будем надеяться, что пути одинаковые всегда
  
  initDirFromReal :: FilePath -> FS Dir
  initDirFromReal fp = return (FileSystemFunctions.Dir [] [] fp)
  
  toKV :: Dir -> FS (FilePath, Dir)
  toKV d@FileSystemFunctions.Dir{path = p} = return (p,d)
  
  getDirectoryContentsPathMy :: FilePath -> IO [FilePath]
  getDirectoryContentsPathMy path = map (path </>) . filter (`notElem` [".", ".."]) <$> getDirectoryContents path
  
  fileDirContentsMy :: FilePath -> FS ([FilePath], [FilePath])
  fileDirContentsMy path -- будем считать, что это запускается от мапы, в которой уже лежт эта директория
   = do
    contents <- liftIO $ getDirectoryContentsPathMy path
    files <- liftIO $ filterM doesFileExist contents
    dirs <- liftIO $ filterM doesDirectoryExist contents
    return (files, dirs)
  
  findAllFilesMy :: FilePath -> FS ([FilePath])
  findAllFilesMy path = do
    fs@FileSystemFunctions.FileSystem {..} <- get
    (files, dirs) <- fileDirContentsMy path
    myfiles <- mapM initFileFromReal files
    mydirs <- mapM initDirFromReal dirs
    --let newMapDir = foldr (\d acc -> HM.insert d (Types.Dir [] [] d) acc) HM.empty mydirs
    mydirst <- mapM toKV mydirs
    let newMapDir = HM.fromList mydirst
    let newDir = FileSystemFunctions.Dir mydirs myfiles path
    let newMapDir = HM.delete path newMapDir
    let newMapDir = HM.insert path newDir newMapDir
    let newFs = FileSystem nowDir initDir newMapDir
    nestedFiles <- mapM findAllFilesMy dirs
    return (files ++ concat nestedFiles)
  
  init :: String -> FS String
  init dir = do
    fs@FileSystem {..} <- get
    let d = (FileSystemFunctions.Dir [] [] dir)
    let newMapDir = HM.insert dir d mapDir
    let newFs = FileSystem dir d newMapDir
    put newFs
    return ""
  
  data FileSystem =
    FileSystem
      { nowDir  :: String
      , initDir :: Dir
      , mapDir  :: HM.Map String Dir
      }
  
  data Dir =
    Dir
      { folders :: [Dir]
      , files   :: [File]
      , path    :: String
      }
  
  data File =
    File
      { name  :: String
      , fPath :: String
      }