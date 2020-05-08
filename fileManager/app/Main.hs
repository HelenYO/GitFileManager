module Main where

import qualified Data.HashMap              as HM
import           FileSystemFunctions
import           Functions
import           Types

import           Data.Foldable             (foldMap)
import           Data.String.Conversions
import           Data.Text                 (Text, pack)

import           Options.Applicative
import           System.Console.Haskeline
import           System.Directory

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State

cmdParser :: Parser (FS String)
cmdParser =
  subparser
    (command "ls" lsInfo <> command "cd" cdInfo <> command "find-file" findInfo <> command "cat" catInfo <>
     command "info" infInfo <>
     command "create-file" mkFileInfo <>
     command "create-folder" mkDirInfo)
  where
    lsInfo = info (pure lsFunc) (progDesc "ls")
    cdInfo =
      info (helper <*> (cdFunc <$> (CdOptions <$> argument str (metavar "PATH" <> help "cd help")))) (progDesc "cd")
    findInfo =
      info
        (helper <*> (findFunc <$> (FindOptions <$> argument str (metavar "PATH" <> help "find-file help"))))
        (progDesc "find-file")
    catInfo =
      info (helper <*> (catFunc <$> (CatOptions <$> argument str (metavar "PATH" <> help "cat help")))) (progDesc "cat")
    infInfo =
      info
        (helper <*> (infFunc <$> (InfOptions <$> argument str (metavar "PATH" <> help "info help"))))
        (progDesc "info")
    mkFileInfo =
      info
        (helper <*> (mkFileFunc <$> (MkFileOptions <$> argument str (metavar "PATH" <> help "create-file help"))))
        (progDesc "create-file")
    mkDirInfo =
      info
        (helper <*> (mkDirFunc <$> (MkDirOptions <$> argument str (metavar "PATH" <> help "create-folder help"))))
        (progDesc "create-folder")

toText :: [FilePath] -> Text
toText = foldMap pack

main :: IO ()
main = do
  let start = "."
  setCurrentDirectory start
  let newfs = FileSystem start start HM.empty HM.empty
  (tmp, fst) <- runStateT (FileSystemFunctions.init start) newfs
  (tmp, fs) <- runInputT defaultSettings $ runStateT loop fst
  liftIO $ putStrLn ""

loop :: StateT FileSystem (InputT IO) ()
loop = do
  minput <- fmap words <$> lift (getInputLine "%: ")
  case minput of
    Nothing -> return ()
    Just args
      | args == ["quit"] || args == ["q"] -> liftIO $ putStr ""
      | null args -> loop
      | otherwise ->
        case execParserPure (prefs idm) pinfo args of
          Success io -> do
            fs <- get
            (str, newFs) <- liftIO $ runStateT io fs
            liftIO $ putStrLn str
            put newFs
            loop
          Failure failure -> do
            liftIO $ putStrLn "error"
            loop
          CompletionInvoked _ -> do
            liftIO $ putStrLn ""
            loop
  where
    pinfo = info (helper <*> cmdParser) fullDesc
----------------------------------------------------------------------------------------------
