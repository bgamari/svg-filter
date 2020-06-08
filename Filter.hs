{-# LANGUAGE OverloadedStrings, TemplateHaskell, RankNTypes #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.State
import Control.Monad.IO.Class
import Data.Monoid hiding (Last)
import Data.Foldable (foldMap)
import System.Process
import System.Environment (getArgs)
import System.FilePath
import System.IO
import Prelude

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Error
import Data.Default
import Control.Lens hiding (Action)
import Data.Attoparsec.Text
import Text.Pandoc.Definition
import Text.Pandoc.JSON
import Text.Pandoc.Walk

import Inkscape

data FilterState = FilterState { _figNum :: Int
                               , _visLayers :: M.Map FilePath (M.Map LayerLabel Opacity)
                               }
makeLenses ''FilterState

instance Default FilterState where
    def = FilterState {_figNum = 0, _visLayers = mempty}

main = do
    args <- getArgs
    case args of
      "notes":_ -> mainNotes
      _         -> mainTalk

mainTalk :: IO ()
mainTalk =
    toJSONFilter $ \doc -> onFailure doc $ evalStateT (filt doc) def
  where
    filt :: Pandoc -> StateT FilterState (ExceptT String IO) Pandoc
    filt =
      walkM walkFilters
      >=> walkM (lift . svgToPdf)
      >=> return . walk filterNotes

mainNotes :: IO ()
mainNotes = toJSONFilter filterForNotes

visLayersFor :: FilePath -> Lens' FilterState (M.Map LayerLabel Opacity)
visLayersFor fname = visLayers . at fname . non M.empty

onFailure :: MonadIO m => a -> ExceptT String m a -> m a
onFailure def action =
    runExceptT action >>= either (\e->liftIO (hPutStrLn stderr e) >> return def) return

walkFilters :: MonadIO m => Inline -> StateT FilterState m Inline 
walkFilters blk@(Image attrs contents (fname,alt)) = onFailure blk $ do
    (contents', filters) <- partitionEithers `fmap` mapM findFilterDef contents
    figNum += 1
    case filters of
      [] -> return $ Image attrs contents (fname, alt)
      _  -> do
        n <- use figNum
        let f = foldl (.) id filters
        let fnameNew = replaceBaseName fname' (takeBaseName fname' <> suffix)
              where suffix = "-" <> show n
        runFilter fname' fnameNew f
        return $ Image attrs contents' (T.pack fnameNew, alt)
  where
    findFilterDef :: Monad m => Inline -> ExceptT String (StateT FilterState m) (Either Inline SvgFilter)
    findFilterDef (Code _ s) = do
        filt <- hoistEither $ parseOnly parseFilter s
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
            return $ Right $ opacityFilter . visFilter
          Scale s -> return $ Right $ scale s
    findFilterDef x = return $ Left x
    fname' = T.unpack fname
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
    
svgToPdf :: Inline -> ExceptT String IO Inline 
svgToPdf (Image attrs contents (fname,alt)) | "svg" `isExtensionOf` fname' = do
    let fnameNew = replaceExtension fname' "pdf"
    liftIO $ callProcess "inkscape" [fname', "--export-pdf", fnameNew]
    return $ Image attrs contents (T.pack fnameNew, alt)
  where fname' = T.unpack fname
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
