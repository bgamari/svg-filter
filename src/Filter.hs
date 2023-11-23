{-# LANGUAGE OverloadedStrings, TemplateHaskell, RankNTypes #-}

module Filter
    ( walkFilters
    , filterNotes
    , filterForNotes
      -- * Testing
    , parseFilter
    , parseItem
    , selectItem
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.State
import Data.Monoid hiding (Last)
import Data.Foldable (foldMap)
import System.Directory
import System.FilePath
import System.IO
import Prelude

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Error
import Data.Default
import Control.Lens hiding (Action, (<.>))
import Data.Attoparsec.Text
import Text.Pandoc.Definition
import Text.Pandoc.JSON
import Text.Pandoc.Walk
import qualified Text.XML.Lens as XML

import Inkscape
import StringMatch

data Item = MatchLabel StringMatch Item
          | MatchId StringMatch Item
          | This
          deriving (Eq, Ord, Show)

matchAll :: Item
matchAll = MatchId (WildcardStar EndMatch) This

data Action = Reset
            | ShowLast
            | Show Item     -- add to visible items
            | Hide Item     -- remove from visible items
            | SetOpacity Float Item  -- set opacity
            | HideOnce Item -- hide for just this frame
            | ShowOnce Item -- show for just this frame
            | SetOpacityOnce Float Item -- set opacity for just this frame
            deriving (Show, Eq, Ord)

type ItemVisibilities = M.Map Item Opacity

type ImageInfo = M.Map Item Opacity

data FilterState = FilterState { _figNum :: Int
                               , _visLayers :: M.Map FilePath ImageInfo
                               }

instance Default FilterState where
    def = FilterState {_figNum = 0, _visLayers = mempty}

makeLenses ''FilterState

type M m = ExceptT String (StateT FilterState m)

outDir :: FilePath
outDir = ".svg-filter"

visItemsFor :: FilePath -> Lens' FilterState ItemVisibilities
visItemsFor fname = visLayers . at fname . non M.empty

onFailure :: MonadIO m => a -> ExceptT String m a -> m a
onFailure def action =
    runExceptT action >>= either (\e->liftIO (errLn $ T.pack e) >> return def) return

mapFileName :: (String -> String) -> FilePath -> FilePath
mapFileName f fpath = (takeDirectory fpath <> fname') <.> takeExtensions fpath
  where fname' = f $ dropExtensions $ takeFileName fpath

walkFilters :: MonadIO m => Inline -> StateT FilterState m Inline
walkFilters blk@(Image attrs contents (fname,alt)) = onFailure blk $ do
    (contents', filters) <- partitionEithers `fmap` mapM findFilterDef contents
    figNum += 1
    case filters of
      [] -> return $ Image attrs contents (fname, alt)
      _  -> do
        n <- use figNum
        let fnameNew = outDir </> mapFileName (<> ("-"<>show n)) fname'
        liftIO $ createDirectoryIfMissing False outDir
        liftIO $ hPutStrLn stderr $ show filters
        svg <- readSvg fname'
        liftIO $ hPutStrLn stderr $ show $ toListOf (XML.root . deep (XML.attr "id")) svg
        s <- use (visItemsFor fname')
        let f = foldl (>=>) pure $ map runAction $ concat filters
        svg' <- zoom (visItemsFor fname') (f svg)
        writeSvg fnameNew svg'
        return $ Image attrs contents' (T.pack fnameNew, alt)
  where
    fname' = T.unpack fname
    findFilterDef :: Monad m => Inline -> M m (Either Inline [Action])
    findFilterDef (Code _ s) = fmap Right $ hoistEither $ parseOnly parseFilter s
    findFilterDef x = return $ Left x

walkFilters inline = return inline

runAction :: Monad m => Action -> Svg -> ExceptT String (StateT ImageInfo m) Svg
runAction Reset svg = put mempty >> return svg
runAction ShowLast svg = do
    s <- get
    let f = appEndo $ foldMap (\(a,b) -> Endo $ setItemOpacity a b) (M.toList s)
    return $ f svg
runAction (Show item) svg = do
    at item .= Just 1
    checkItem item svg
    return $ setItemOpacity item 1 svg
runAction (Hide item) svg = do
    at item .= Just 0
    checkItem item svg
    return $ setItemOpacity item 0 svg
runAction (SetOpacity o item) svg = do
    at item .= Just o
    checkItem item svg
    return $ setItemOpacity item o svg
runAction (ShowOnce item) svg = do
    checkItem item svg
    return $ set (selectItem item . opacity) 1 svg
runAction (HideOnce item) svg = do
    checkItem item svg
    return $ set (selectItem item . opacity) 0 svg
runAction (SetOpacityOnce o item) svg = do
    checkItem item svg
    return $ set (selectItem item . opacity) o svg

checkItem :: Monad m => Item -> Svg -> ExceptT String m ()
checkItem item svg = do
    when (lengthOf (selectItem item) svg == 0)
        $ throwE $ "item selector "<>show item<>" matches no elements"

selectItem :: Item -> Traversal' Svg Element
selectItem item = XML.root . deep (go item)
  where
    go (MatchId r rest) = XML.attributeSatisfies "id" (attrMatches r) . go rest
    go (MatchLabel r rest) = XML.attributeSatisfies labelAttr (attrMatches r) . go rest
    go This = id

    attrMatches :: StringMatch -> T.Text -> Bool
    attrMatches re = matches re . T.unpack

setItemOpacity :: Item -> Opacity -> Svg -> Svg
setItemOpacity item = set (selectItem item . opacity)

---------------------------------------------------------
-- Action parser
---------------------------------------------------------

parseItem :: Parser Item
parseItem = anId <|> aLabel <|> pure This
  where
    anId = do
        char '#'
        re <- regex
        rest <- continued
        return $ MatchId re rest
    aLabel = do
        re <- regex
        rest <- continued
        return $ MatchLabel re rest

    continued = fromMaybe This <$> optional nested
      where
        nested = char '/' *> parseItem

    regex :: Parser StringMatch
    regex = foldr ($) EndMatch <$> some (choice [oneWildcard, wildcard, litChar])
    oneWildcard = Wildcard <$ char '?'
    wildcard = WildcardStar <$ char '*'
    litChar = LitChar <$> satisfy (inClass "-a-zA-Z0-9")

parseAction :: Parser Action
parseAction =
    choice [ string "!reset" >> return Reset
           , string "!show-all" >> return (Show matchAll)
           , string "!hide-all" >> return (Hide matchAll)
           , string "!last" >> return ShowLast
           , do char '+'
                x <- parseItem
                mOpacity <- optional $ do
                    char '='
                    realToFrac <$> double
                case mOpacity of
                  Just o  -> return $ SetOpacity o x
                  Nothing -> return (Show x)
           , char '-' >> fmap Hide parseItem
           , string "!hide-once:" >> fmap HideOnce parseItem
           , do x <- parseItem
                guard (x /= This)
                mOpacity <- optional $ do
                    char '='
                    realToFrac <$> double
                case mOpacity of
                  Just o  -> return $ SetOpacityOnce o x
                  Nothing -> return (ShowOnce x)
           ]

parseFilter :: Parser [Action]
parseFilter = do
    string "svg-filter:"
    skipSpace
    many $ parseAction <* skipSpace

filterNotes :: Block -> [Block]
filterNotes (OrderedList (0,_,_) _) = []
filterNotes blk = [blk]

filterForNotes :: Pandoc -> Pandoc
filterForNotes (Pandoc m body) = Pandoc m (filter f body)
  where
    f (OrderedList (0,_,_) _) = True
    f (Header _ _ _) = True
    f blk = False
