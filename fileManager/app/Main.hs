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

psevdolsFunc :: LsOptions -> FS ()
psevdolsFunc str = lsFunc

cmdParser :: Parser (FS ())
cmdParser =
  subparser
    (command "ls" lsInfo --сейчас никак не использует аргумент, заменить 1 строчку
      )
  where
    lsInfo =
      info
        (helper <*> (psevdolsFunc <$> (LsOptions <$> argument str (metavar "PATH" <> help "ls help"))))
        (progDesc "ls")

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
  setCurrentDirectory "/Users/elena/Desktop/files/files2"
  let newfs = FileSystem "/Users/elena/Desktop/files" (Dir [] [] "/Users/elena/Desktop/files") HM.empty
  (tmp, fs) <- runStateT (FileSystemFunctions.init "/Users/elena/Desktop/files") newfs
  runInputT defaultSettings loop

loop :: InputT IO ()
--loop :: InputT (StateT [String] IO) ()
loop = do
  minput <- fmap words <$> getInputLine "%: "
  case minput of
    Nothing -> return ()
    Just args
      | args == ["quit"] || args == ["q"] -> liftIO $ putStr ""
      | null args -> loop
      | otherwise ->
        case execParserPure (prefs idm) pinfo args of
          Success io -> do
             liftIO (io) >> loop
--            (str, newFs) <- runStateT (io) fs
--            liftIO $ io >> loop
--          --(str, newFs) <- runStateT (func ["str"]) fs
          Failure failure -> do
            liftIO $ putStrLn "error"
            loop
          CompletionInvoked _ -> do
            liftIO $ putStrLn ""
            loop
  where
    pinfo = info (helper <*> cmdParser) fullDesc
----------------------------------------------------------------------------------------------
-- EXAMPLE
-- https://github.com/mineo/edabo/blob/814e2464ddba509039655d399b56d23ed3b12435/src/Edabo/CmdLine.hs
--import           Control.Applicative    ((<$>), (<*>), many)
--import           Data.Monoid            ((<>))
--import           Data.Monoid         (Monoid (..))
--import           System.Exit                    (exitFailure, exitSuccess)
--import           Options.Applicative    (Parser, argument, command, execParser,
--                                         fullDesc, header, help, helper, info,
--                                         long, metavar, optional, progDesc,
--                                         pure, short, str, strOption, subparser,
--                                         switch)
--
--data Command
--  = DeletePlaylist
--
--data Options = Options
--  { optVerbose :: Bool
--  , optCommand :: Command }
--
--deletePlaylist ::  IO ()
--deletePlaylist  = do
--  putStrLn "Server starting..."
--
--data DeletePlaylistOptions = DeletePlaylistOptions
--  { optPlaylistToDeleteName :: String
--  }
--
--parseDeletePlaylist :: Parser Command
--parseDeletePlaylist = pure DeletePlaylist
--
--subCommandParser :: Parser Command
--subCommandParser = subparser
--           (makeCommand "delete" parseDeletePlaylist "delete a playlist"
--           )
--           where withHelper f = helper <*> f
--                 makeCommand name f desc = command name (info (withHelper f)
--                                                         (progDesc desc))
--
--globalParser :: Parser Options
--globalParser = Options
--               <$> switch (short 'v'
--                           <> long "verbose")
--               <*> subCommandParser
--
--handleArgs :: IO ()
--handleArgs = execParser opts >>= run
--  where opts = info (helper <*> globalParser)
--          ( fullDesc
--          <> progDesc "playlist saver & loader"
--          <> header "edabo"
--          )
--
--run :: Options -> IO ()
--run Options {optCommand = cmd} = runCmd >>= quit
--  where runCmd = case cmd of
--                   DeletePlaylist -> deletePlaylist
--        quit e = do
--          exitSuccess
--
--main :: IO ()
--main = do
--  handleArgs
----------------------------------------------------------------------------------------------
-- MY VERSION
--import FunctionLS
--
--import Options.Applicative
--import Data.Semigroup ((<>))
--
--import Data.String
--import Options.Applicative.Types (ReadM, readerAsk)
--import Control.Monad(join, unless)
--
--type Host = String
--type Port = Integer
--
--ls :: IO ()
--ls = lsFunc
--
--opts :: Parser (IO ())
--opts = subparser commands
--  where
--    lsCmd :: ParserInfo (IO ())
--    lsCmd = info (pure ls) idm
--
--    commands :: Mod CommandFields (IO ())
--    commands = command "ls" lsCmd
--
--cmdProgram :: IO ()
--cmdProgram = join $ execParser parser
--  where
--    parser :: ParserInfo (IO ())
--    parser = info opts idm
--
--main :: IO ()
--main = do
--  cmdProgram
--  main
----------------------------------------------------------------------------------------------
--EXAMPLE COMMANDS
-- https://github.com/chengzh2008/restish-todo/blob/d84afa0ad6cfb9773c75f74e99b77e834e2c8528/app/Main.hs
--type Host = String
--type Port = Integer
--
--data Command = Serve Host Port
--
--newtype Options = Options { cmd:: Command }
--
---- | Start up the server and serve request
--server :: IO ()
--server = putStrLn "Server starting..."
--
--monitor :: IO ()
--monitor = putStrLn "Monitoring..."
--
---- | CLI options parser
--opts :: Parser (IO ())
--opts = subparser commands
--  where
--    serverCmd :: ParserInfo (IO ())
--    serverCmd = info (pure server) idm
--
--    monitorCmd :: ParserInfo (IO ())
--    monitorCmd = info (pure monitor) idm
--
--    commands :: Mod CommandFields (IO ())
--    commands = command "server" serverCmd
--               <> command "monitor" monitorCmd
--
--main :: IO ()
--main = join $ execParser parser
--  where
--    parser :: ParserInfo (IO ())
--    parser = info opts idm
----------------------------------------------------------------------------------------------
----EXAMPLE PARAMETERS
--import Options.Applicative
--import Data.Semigroup ((<>))
--
--data Sample = Sample
--  { hello      :: String
--  , quiet      :: Bool
--  , enthusiasm :: Int }
--
--sample :: Parser Sample
--sample = Sample
--      <$> strOption
--          ( long "hello"
--         <> metavar "TARGET"
--         <> help "Target for the greeting" )
--      <*> switch
--          ( long "quiet"
--         <> short 'q'
--         <> help "Whether to be quiet" )
--      <*> option auto
--          ( long "enthusiasm"
--         <> help "How enthusiastically to greet"
--         <> showDefault
--         <> value 1
--         <> metavar "INT" )
--
--
--main :: IO ()
--main = greet =<< execParser opts
--  where
--    opts = info (sample <**> helper)
--      ( fullDesc
--     <> progDesc "Print a greeting for TARGET"
--     <> header "hello - a test for optparse-applicative" )
--
--greet :: Sample -> IO ()
--greet (Sample h False n) = putStrLn $ "Hello, " ++ h ++ replicate n '!'
--greet _ = return ()
----------------------------------------------------------------------------------------------
--
--import Lib
--import FunctionLS
--
--main :: IO ()
--main = lsFunc
----------------------------------------------------------------------------------------------
