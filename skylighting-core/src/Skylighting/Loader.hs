-- | This module provides routines to load syntax definitions from disk
-- files.
module Skylighting.Loader ( loadSyntaxFromFile
                          , loadSyntaxesFromDir
                          )
                          where

import Control.Applicative ((<$>))
import Control.Monad (filterM, foldM)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>), mempty)
import System.Directory (listDirectory, doesFileExist)
import System.FilePath ((</>), takeExtension)

import Skylighting.Types (SyntaxMap, Syntax)
import Skylighting.Parser (addSyntaxDefinition, parseSyntaxDefinition)

syntaxFileExtension :: String
syntaxFileExtension = ".xml"

isSyntaxFile :: FilePath -> Bool
isSyntaxFile = (== syntaxFileExtension) . takeExtension

-- | Loads a syntax definition from the specifed file path. The file
-- path must refer to a file containing an XML Kate syntax definition.
loadSyntaxFromFile :: FilePath -> IO (Either String Syntax)
loadSyntaxFromFile path = do
    result <- parseSyntaxDefinition path
    case result of
        Left e -> return $ Left $ "Error parsing file " <> show path <> ": " <> e
        Right s -> return $ Right s

-- | Loads all syntax definitions from the specified directory by
-- looking for files with an ".xml" extension. This function assumes
-- such files are Kate XML syntax definitions, so XML files with
-- unexpected contents will cause a parsing error returned as a 'Left'.
loadSyntaxesFromDir :: FilePath -> IO (Either String SyntaxMap)
loadSyntaxesFromDir path = runExceptT $ do
    files <- liftIO $ syntaxFiles path

    let loadSyntax sMap file = do
            s <- ExceptT $ loadSyntaxFromFile file
            return $ addSyntaxDefinition s sMap

    foldM loadSyntax mempty files

syntaxFiles :: FilePath -> IO [FilePath]
syntaxFiles dir = do
    entries <- listDirectory dir
    let absEntries = (dir </>) <$> filter isSyntaxFile entries
    filterM doesFileExist absEntries
