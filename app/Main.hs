module Main where

import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Replace as RT
import Data.Void
import Replace.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import System.Directory
import System.FilePath.Posix

main = do
    args <- getArgs
    proccessArgs args

-- proccessArgs :: [String] -> IO [()]
proccessArgs (arg1 :arg2 : []) = processMdFile arg1 arg2

-- assumes dest is always liks /home/<>/obs/processed/
-- processMdFile :: FilePath -> FilePath -> IO ()
processMdFile :: FilePath -> String -> IO [()]
processMdFile src dest = do
    filesdir <- getDirectoryContents src 
    _ <- removeDirectoryRecursive dest 
    _ <- createDirectory dest
    _ <- setCurrentDirectory dest
    _ <- createDirectory "images/"
    mapM (\f -> copyFile (src ++ "/" ++ f) ("images/" ++ rScore f)) (images filesdir)
    mapM (\f -> reformatImage (src ++ "/" ++ f) f) (mdFiles filesdir)
    
    where 
        mdFiles = filter (\f ->  (takeExtension f) == ".md" )
        images = filter (\f ->  (takeExtension f) == ".png" )

reformatImage :: FilePath -> FilePath -> IO ()
reformatImage src destFile = do
    mdFileText <-  T.readFile src
    let bracevar = chunk "[[" *> manyTill anySingle (chunk ".png]]") :: Parsec Void String String
    T.writeFile destFile $ T.pack $ streamEdit bracevar (\p -> "[" ++ rScore p ++ "]" ++ "(" ++ "/processed/images/" ++ (rScore p)  ++ ".png)") $ T.unpack mdFileText
        

rScore :: String -> String
rScore str = 
    streamEdit space (const "_") str
    where 
        space = chunk " " :: Parsec Void String String