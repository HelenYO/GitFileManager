module Types where

import           Control.Monad
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.State
import qualified Data.HashMap              as HM
import           System.Directory
import           System.FilePath
import           System.FilePath.Find
import           System.FilePath.Posix

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