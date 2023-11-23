{-# LANGUAGE OverloadedStrings, TemplateHaskell, RankNTypes #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.State
import System.Process
import System.Environment (getArgs)
import System.FilePath
import Prelude

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.E

import Control.Error
import Data.Default
import Text.Pandoc
import Text.Pandoc.Walk

import Filter
import Inkscape

filterPandoc :: (MonadIO m, PandocMonad m)
             => (Pandoc -> m Pandoc) -> m ()
filterPandoc filter =
        liftIO BS.getContents
    >>= readJSON def . T.E.decodeUtf8
    >>= filter
    >>= writeJSON def
    >>= liftIO . BS.putStr . T.E.encodeUtf8

main :: IO ()
main = do
    args <- getArgs
    runIOorExplode $ case args of
      "run":svg:filt:[] -> liftIO $ runFilters svg filt
      "notes":_         -> mainNotes
      _                 -> mainTalk

mainTalk :: PandocIO ()
mainTalk =
    flip evalStateT def $ filterPandoc $
      walkM walkFilters
      >=> walkM (lift . svgToPdf)
      >=> return . walk (foldMap filterNotes)

mainNotes :: PandocIO ()
mainNotes = filterPandoc $ return . filterForNotes

svgToPdf :: MonadIO m => Inline -> m Inline
svgToPdf (Image attrs contents (fname,alt)) | "svg" `isExtensionOf` fname' = do
    let fnameNew = replaceExtension fname' "pdf"
    liftIO $ callProcess "rsvg-convert" ["-f", "pdf", "-o", fnameNew, fname']
    return $ Image attrs contents (T.pack fnameNew, alt)
  where
    fname' = T.unpack fname
svgToPdf inline = return inline

runFilters :: FilePath -> String -> IO ()
runFilters svgPath filt = exceptT fail return $ do
    svg <- readSvg svgPath
    actions <- hoistEither $ parseFilters (T.pack filt)
    svg' <- ExceptT $ flip evalStateT mempty $ runExceptT $ runActions actions svg
    writeSvg "out.svg" svg'
