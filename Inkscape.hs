{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Inkscape
    ( SvgFilter
    , LayerLabel
    , Opacity
    , layersLabelled
    , layerOpacity
    , hideLayers
    , showOnlyLayers
    , scale
    , runFilter
    ) where

import Prelude hiding (FilePath)

import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import qualified Data.Map as M
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import qualified Data.Text as T
import qualified Text.XML as Xml
import           Data.Text (Text)
import Text.XML.Lens
import Control.Error
import Data.Default
import Data.Default
import Numeric.Lens       
import Data.Text.Lens

import qualified Filesystem.Path as Path
import Filesystem.Path.CurrentOS (FilePath, decodeString, encodeString, toText)

type SvgFilter = Document -> Document
type LayerLabel = Text
type Opacity = Float
type Layer = Element

inkscape :: Name -> Name
inkscape = _nameNamespace ?~ "http://www.inkscape.org/namespaces/inkscape"

svg :: Name -> Name
svg = _nameNamespace ?~ "http://www.w3.org/2000/svg"

layerLabel :: Lens' Layer (Maybe LayerLabel)
layerLabel = attrs . at (inkscape "label")

allLayers :: Document -> [LayerLabel]
allLayers doc = catMaybes $ doc ^.. traverseLayers . layerLabel

traverseLayers :: Traversal' Document Layer
traverseLayers =
    root
    . entire . filtered (views name (== svg "g"))
    . attributeIs (inkscape "groupmode") "layer"

layersLabelled :: LayerLabel -> Traversal' Document Layer
layersLabelled label = 
    traverseLayers . filtered match
  where match el = el ^. layerLabel == Just label

showAllGroups :: Document -> Document
showAllGroups = traverseLayers . attrs . at "style" .~ Nothing

hideLayers :: [LayerLabel] -> SvgFilter
hideLayers layers doc =             
    let match el = (el ^. layerLabel) `elem` map Just layers
    in showAllGroups doc
       & traverseLayers
       . filtered match
       . style "display" ?~ "none"

showOnlyLayers :: [LayerLabel] -> SvgFilter
showOnlyLayers showLayers doc =             
    let match el = (el ^. layerLabel) `notElem` map Just showLayers
    in showAllGroups doc
       & traverseLayers
       . filtered match
       . style "display" ?~ "none"

layerOpacity :: Lens' Layer Opacity
layerOpacity = style "opacity" . iso to from
  where
    to Nothing  = 1
    to (Just x) = maybe 1 id $ readMay $ T.unpack x
    from 1 = Nothing
    from x = Just $ T.pack $ show x

type StyleAttr = Text

style :: StyleAttr -> Lens' Element (Maybe Text)
style s = attribute "style" . non T.empty . style' . at s

style' :: Iso' Text (M.Map StyleAttr Text)
style' = iso to from
  where
    splitKeyValue x = case T.splitOn ":" x of
                        [k,v]     -> M.singleton k v
                        otherwise -> M.empty
    to = M.unions . map splitKeyValue . T.splitOn ";" 
    from = T.intercalate ";" . map (\(k,v)->k<>":"<>v) . M.toList
        
scale :: Double -> Document -> Document
scale s doc = root . nodes %~ scaleNodes
            $ root . attr "width" . float %~ (/2)
            $ root . attr "height" . float %~ (/2)
            $ doc
  where
    float :: Prism' Text Float
    float = from packed . prism' show readMay
    scaleNodes :: [Node] -> [Node]
    scaleNodes nodes =
      [NodeElement $ Element "{http://www.w3.org/2000/svg}g" scaleAttr nodes]
    scaleAttr = M.singleton "transform" (T.pack $ "scale("++show s++")")

runFilter :: MonadIO m => FilePath -> FilePath -> SvgFilter -> EitherT String m ()
runFilter inFile outFile transform = do
    doc <- liftIO $ Xml.readFile def inFile
    --let notFound = filter (\l->l `notElem` allLayers doc) showLayers
    --when (not $ null notFound) $ lift $ putStrLn $ "couldn't find layers: "++show notFound
    liftIO $ Xml.writeFile def outFile (transform doc)
