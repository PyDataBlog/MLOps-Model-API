-----------------------------------------------------------------------------
-- |
-- Module      : GitBak
-- Copyright   : (c) Agorgianitis Loukas, 2015
-- License     : MIT
--
-- Maintainer  : Agorgianitis Loukas <agorglouk@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Main of gitbak executable
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Options.Applicative
import System.FilePath
import System.Directory
import System.IO
import System.Process
import System.Exit
import qualified Control.Lens as Ln
import qualified Network.Wreq as Wr
import qualified Data.ByteString.Char8 as B8

---------------------------------------------------------------------------
-- Command Line Options
---------------------------------------------------------------------------
-- Helper
withInfo :: Parser a -> String -> ParserInfo a
opts `withInfo` desc = info (helper <*> opts) $ progDesc desc

data Options = Options { user :: String, ghAPIKey :: Maybe String }

-- Main parser
parseOptions :: Parser Options
parseOptions = Options  
    <$> argument str (metavar "USER" <> help "The GitHub user to clone repos from")
    <*> optional (argument str (metavar "APIKEY" <> help "The optional GitHub API key to enable more API requests per minute"))

-- The main description generator
parseOptionsInfo :: ParserInfo Options
parseOptionsInfo = info (helper <*> parseOptions)
                        (fullDesc
                      <> header "GitBak - A GitHub mass clone utility")

---------------------------------------------------------------------------
-- Deserializing
---------------------------------------------------------------------------
-- The data structure that holds a fetched repo information
data RepoInfo = RepoInfo { repoName :: String , repoLink :: String } deriving Show

instance FromJSON RepoInfo where
    parseJSON (Object v) = RepoInfo    <$>
                           v .: "name" <*>
                           v .: "clone_url"
    parseJSON _ = mempty

---------------------------------------------------------------------------
-- Actions
---------------------------------------------------------------------------
-- Gathers a RepoInfo list for the given username, using the optional API key
gatherRepoList :: String -> Maybe String -> IO [RepoInfo]
gatherRepoList name apiKey = do
    let initUrl = "https://api.github.com/users/" ++ name ++ "/repos"
    let opts = Wr.defaults
    let opts2 = case apiKey of
                    Just key -> opts Ln.& Wr.header "Authorization" Ln..~ [B8.pack $ "token " ++ key]
                    Nothing -> opts
    let getRepoLinks url progress =
         Wr.getWith opts2 url >>= (\x ->
            let body = x Ln.^. Wr.responseBody
                restLink = x Ln.^? Wr.responseLink "rel" "next" . Wr.linkURL
            in case decode body :: Maybe [RepoInfo] of
                Nothing -> return []
                Just v -> do
                    let newProgress = progress + length v
                    putStr $ "\rTotal repos: " ++ show newProgress
                    hFlush stdout
                    rest <- case restLink of
                                 Nothing -> return []
                                 Just l  -> getRepoLinks (B8.unpack l) newProgress
                    return $ v ++ rest)
    putStr $ "Total repos: " ++ show (0 :: Int)
    hFlush stdout
    repoLinks <- getRepoLinks initUrl 0
    putStr "\n"
    return repoLinks

-- Clones a git repository using the git executable
cloneGitRepo :: RepoInfo -> IO ExitCode
cloneGitRepo inf = system $ "git clone " ++ repoLink inf ++ " " ++ repoName inf

-- Returns a list of all the given directory contents recursively
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- mapM (\x -> do
        let path = topdir </> x
        isDir <- doesDirectoryExist path
        if isDir
            then (++ [path]) <$> getRecursiveContents path
            else return [path]
        ) properNames
    return (concat paths)

-- Sets write permission of a file or folder to true
makeWritable :: FilePath -> IO ()
makeWritable path = do
    p <- getPermissions path
    setPermissions path (p {writable = True})

-- Archives a given folder using the tar util
zipFolder :: FilePath -> IO ExitCode
zipFolder name = 
    system $ "tar czvf " ++ name ++ ".tar.gz " ++ name

---------------------------------------------------------------------------
-- Entrypoint
---------------------------------------------------------------------------
main :: IO ()
main = do
    opts <- execParser parseOptionsInfo
    putStrLn "Fetching repos..."
    repos <- gatherRepoList (user opts) (ghAPIKey opts)        
    forM_ repos (\x -> do
        let name = repoName x
        -- Clone
        putStrLn $ "Cloning " ++ name ++ "..."
        _ <- cloneGitRepo x
        -- Zip
        putStrLn $ "Archiving " ++ name ++ "..."
        _ <- zipFolder name
        -- Delete cloned folder
        getRecursiveContents name >>= mapM_ makeWritable
        removeDirectoryRecursive name)

