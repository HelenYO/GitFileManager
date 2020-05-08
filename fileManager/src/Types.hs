

module Types where

import           Control.Monad
--import Functions
import           Control.Monad.Trans.State
import qualified Data.HashMap              as HM
import           System.FilePath.Posix
import           System.Directory
import           System.FilePath
import           System.FilePath.Find
import Control.Monad.IO.Class (liftIO)


--type FS a = StateT FileSystem IO a
--
--func2 :: FS ()
--func2 = do
--  res <- func ["1"]
--  return ()
--
--main :: IO ()
--main = do
--  let fs = FileSystem "path" (Directory [] [] "b")
--  (str, newFs) <- runStateT (func ["str"]) fs
--  putStrLn str
--
--func :: [String] -> FS String
--func _ = do
--  fs@FileSystem{..} <- get
--
--func1 :: Int -> Int -> FS String
----  newFS
----  put newFS
--  return "aaa"


newtype CdOptions =
  CdOptions
    { cdDir :: FilePath
    }

newtype LsOptions =
  LsOptions
    { lsName :: String
    }

newtype CatOptions =
  CatOptions
    { catName :: String
    }

newtype MkFileOptions =
  MkFileOptions
    { mkfileName :: String
    }

newtype MkDirOptions =
  MkDirOptions
    { mkDir :: String
    }

newtype RmOptions =
  RmOptions
    { rmPath :: String
    }

newtype DirOptions =
  DirOptions
    { dirName :: String
    }

newtype FindOptions =
  FindOptions
    { findName :: String
    }

newtype InfOptions =
  InfOptions
    { infName :: String
    }

data Write2FileOptions =
  Write2FileOptions
    { rewrite :: Bool
    , wrFile  :: String
    , wrData  :: String
    }

data Command
  = Ls LsOptions
  | Cd CdOptions
