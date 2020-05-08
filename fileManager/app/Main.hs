module Main where

import qualified Data.HashMap              as HM
import           FileSystemFunctions
import           FileSystemFunctions       (Dir (..), FileSystem (..))
import           Functions
import           Types

import           Data.Foldable             (foldMap)
import           Data.String.Conversions
import           Data.Text                 (Text, pack)

import           Options.Applicative
import           Options.Applicative       (switch)
import           System.Console.Haskeline
import           System.Directory

import           Control.Monad.IO.Class
import           Control.Monad.Trans.State (runStateT)
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Class

cmdParser :: Parser (FS String)
cmdParser =
  subparser
    (  command "ls" lsInfo
    <> command "cd" cdInfo)
  where
    lsInfo =
        info
          (pure lsFunc)
          (progDesc "ls")
    cdInfo =
          info (helper <*> (cdFunc <$> (CdOptions <$> argument str (metavar "PATH" <> help "cd help")))) (progDesc "cd")
        
--    lsInfo =
--      info
--        (helper <*> (psevdolsFunc <$> (LsOptions <$> argument str (metavar "PATH" <> help "ls help"))))
--        (progDesc "ls")

--psevdolsFunc :: LsOptions -> IO ()
--psevdolsFunc str = lsFunc
--
--cmdParser :: Parser (IO ())
--cmdParser =
--  subparser
--    (command "ls" lsInfo --сейчас никак не использует аргумент, заменить 1 строчку
--      <>
--     command "cd" cdInfo <>
--     command "cat" catInfo <>
--     command "create-file" mkFileInfo <>
--     command "create-folder" mkDirInfo <>
--     command "remove" rmInfo <>
--     command "write-file" wrInfo --есть опция
--      <>
--     command "dir" dirInfo <>
--     command "find-file" findInfo <>
--     command "information" infInfo)
--  where
--    infInfo =
--      info
--        (helper <*> (infFunc <$> (InfOptions <$> argument str (metavar "PATH" <> help "find help"))))
--        (progDesc "find")
--    findInfo =
--      info
--        (helper <*> (findFunc <$> (FindOptions <$> argument str (metavar "PATH" <> help "find-file help"))))
--        (progDesc "find-file")
--    dirInfo =
--      info (helper <*> (dirFunc <$> (DirOptions <$> argument str (metavar "PATH" <> help "dir help")))) (progDesc "dir")
--    wrInfo =
--      info
--        (helper <*>
--         (wr2FileFunc <$>
--          (Write2FileOptions <$> switch (long "rewrite" <> short 'r' <> help "rewrite file") <*>
--           argument str (metavar "PATH" <> help "write-file help") <*>
--           argument str (metavar "PATH" <> help "data help"))))
--        (progDesc "write-file")
--    rmInfo =
--      info
--        (helper <*> (rmFunc <$> (RmOptions <$> argument str (metavar "PATH" <> help "remove help"))))
--        (progDesc "remove")
--    mkDirInfo =
--      info
--        (helper <*> (mkDirFunc <$> (MkDirOptions <$> argument str (metavar "PATH" <> help "create-folder help"))))
--        (progDesc "create-folder")
--    mkFileInfo =
--      info
--        (helper <*> (mkFileFunc <$> (MkFileOptions <$> argument str (metavar "PATH" <> help "create-file help"))))
--        (progDesc "create-file")
--    catInfo =
--      info (helper <*> (catFunc <$> (CatOptions <$> argument str (metavar "PATH" <> help "cat help")))) (progDesc "cat")
--    cdInfo =
--      info (helper <*> (cdFunc <$> (CdOptions <$> argument str (metavar "PATH" <> help "cd help")))) (progDesc "cd")
--    lsInfo =
--      info
--        (helper <*> (psevdolsFunc <$> (LsOptions <$> argument str (metavar "PATH" <> help "ls help"))))
--        (progDesc "ls")

toText :: [FilePath] -> Text
toText = foldMap pack

main :: IO ()
main = do
  setCurrentDirectory "/Users/elena/Desktop/files"
  let newfs = FileSystem "/Users/elena/Desktop/files" "/Users/elena/Desktop/files" HM.empty
  (tmp, fst) <- runStateT (FileSystemFunctions.init "/Users/elena/Desktop/files") newfs
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