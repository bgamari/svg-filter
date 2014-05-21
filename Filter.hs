{-# LANGUAGE OverloadedStrings, TemplateHaskell, RankNTypes #-}
import Prelude hiding (FilePath)

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.State
import Control.Monad.IO.Class
import Data.Monoid hiding (Last)
import Data.Foldable (foldMap)
import System.Process
import System.IO hiding (FilePath)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Control.Error
import Data.Default
import Control.Lens hiding (Action)
import Data.Attoparsec.Text
import Text.Pandoc
import Text.Pandoc.Walk
import Filesystem.Path.CurrentOS
import Debug.Trace

import Inkscape

data FilterState = FilterState { _figNum :: Int
                               , _visLayers :: M.Map FilePath (M.Map LayerLabel Opacity)
                               }
makeLenses ''FilterState

instance Default FilterState where
    def = FilterState {_figNum = 0, _visLayers = mempty}

filterPandoc :: MonadIO m => (Pandoc -> m Pandoc) -> m ()
filterPandoc filter =
        liftIO getContents
    >>= return . readJSON def
    >>= filter
    >>= return . writeJSON def
    >>= liftIO . putStr

main = mainTalk

mainTalk :: IO (Either String ())
mainTalk =
    runEitherT $ flip evalStateT def $ filterPandoc $
      walkM walkFilters
      >=> walkM (lift . svgToPdf)
      >=> return . walk filterNotes

mainNotes :: IO ()
mainNotes = filterPandoc $ return . filterForNotes

mapFileName :: (T.Text -> T.Text) -> FilePath -> FilePath
mapFileName f fpath = (directory fpath <> fname') `addExtensions` extensions fpath
  where fname' = fromText $ f $ either (error "invalid file name") id $ toText
               $ dropExtensions $ filename fpath

visLayersFor :: FilePath -> Lens' FilterState (M.Map LayerLabel Opacity)
visLayersFor fname = visLayers . at fname . non M.empty

onFailure :: MonadIO m => a -> EitherT String m a -> m a
onFailure def action =
    runEitherT action >>= either (\e->liftIO (hPutStr stderr e) >> return def) return

walkFilters :: MonadIO m => Inline -> StateT FilterState m Inline 
walkFilters blk@(Image contents (fname,alt)) = onFailure blk $ do
    (contents', filters) <- partitionEithers `fmap` mapM findFilterDef contents
    figNum += 1
    case filters of
      [] -> return $ Image contents (fname, alt)
      _  -> do
        n <- use figNum
        let f = foldl (.) id filters
        let fnameNew = mapFileName (<>T.pack ("-"++show n)) fname'
        runFilter fname' fnameNew f
        return $ Image contents' (encodeString fnameNew, alt)
  where
    fname' = decodeString fname
    findFilterDef :: Monad m => Inline -> EitherT String (StateT FilterState m) (Either Inline SvgFilter)
    findFilterDef (Code _ s) = do
        filt <- hoistEither $ parseOnly parseFilter $ T.pack s
        traceShow filt $ return ()
        case filt of
          Only layers -> do
            lift $ visLayersFor fname' .= foldMap (\l->M.singleton l 1) layers
            return $ Right $ showOnlyLayers $ S.toList layers
          Last layers -> do
            let filterAction :: Action -> M.Map LayerLabel Opacity
                filterAction action =
                    let go (action', name, opacity)
                          | action == action'  = M.singleton name opacity
                          | otherwise          = mempty
                    in foldMap go layers
            lift $ visLayersFor fname' %= \xs->xs `M.union` filterAction Add
                                                  `M.difference` filterAction Remove
            vis <- use $ visLayersFor fname'
            let vis' = vis `M.difference` filterAction HideOnce
                           `M.union` filterAction ShowOnce
                visFilter, opacityFilter :: SvgFilter
                visFilter = showOnlyLayers (M.keys $ vis')
                opacityFilter doc = foldl (\doc' (name,op)->doc & layersLabelled name . layerOpacity .~ op) doc (M.assocs vis')
            traceShow vis' $ return ()
            return $ Right $ opacityFilter . visFilter
          Scale s -> return $ Right $ scale s
    findFilterDef x = return $ Left x
walkFilters inline = return inline

data FilterType = Only (S.Set LayerLabel)
                | Last [(Action, LayerLabel, Opacity)]
                | Scale Double
                deriving (Show)
                
data Action = Add      -- add to visible layers
            | Remove   -- remove from visible layers
            | HideOnce -- hide for just this frame
            | ShowOnce -- show for just this frame
            deriving (Show, Eq, Ord)

layerName :: Parser LayerLabel
layerName = takeWhile1 $ inClass "-a-zA-Z0-9"

parseLayer :: Parser (Action, LayerLabel, Opacity)
parseLayer = do
    action <- choice [ char '+' >> return Add
                     , char '-' >> return Remove
                     , char '!' >> return HideOnce
                     ,             return ShowOnce
                     ]
    name <- layerName
    opacity <- option 1 $ char '=' >> rational
    return (action, name, opacity)

parseFilter :: Parser FilterType
parseFilter = do
    string "filter:" 
    skipSpace
    only <|> last <|> scale
  where
    only = do string "only" >> skipSpace
              Only . S.fromList <$> layerName `sepBy` skipSpace
    last = do string "last" >> skipSpace
              Last <$> parseLayer `sepBy` skipSpace
    scale = do string "scale" >> skipSpace
               Scale <$> rational
    
svgToPdf :: Inline -> EitherT String IO Inline 
svgToPdf (Image contents (fname,alt)) | fname' `hasExtension` "svg" = do
    let fnameNew = replaceExtension fname' "pdf"
    liftIO $ callProcess "inkscape" [fname, "--export-pdf", encodeString fnameNew]
    return $ Image contents (encodeString fnameNew, alt)
  where fname' = decodeString fname
svgToPdf inline = return inline
            
filterNotes :: Block -> Block
filterNotes (OrderedList (0,_,_) _) = Null
filterNotes blk = blk            

filterForNotes :: Pandoc -> Pandoc
filterForNotes (Pandoc m body) = Pandoc m (filter f body)
  where
    f (OrderedList (0,_,_) _) = True
    f (Header _ _ _) = True
    f blk = False
