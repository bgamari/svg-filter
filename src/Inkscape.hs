{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Inkscape
    ( Svg
    , SvgFilter
    , LayerLabel
    , Opacity
    , Element
    , labelAttr
    , readSvg
    , writeSvg
    , byId
    , isLayer
    , layerLabel
    , byLabel
    , setOpacity
    , opacity
    , scale
    ) where

import Prelude

import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import qualified Data.Map as M
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import qualified Data.Text as T
import qualified Text.XML as Xml
import           Data.Text (Text)
import Text.XML.Lens
import Control.Lens
import Control.Error
import Data.Default
import Data.Text.Lens

type Svg = Document
type SvgFilter = Document -> Document
type LayerLabel = Text
type ElementId = Text
type Opacity = Float
type Layer = Element

inInkscapeNs :: Name -> Name
inInkscapeNs = _nameNamespace ?~ "http://www.inkscape.org/namespaces/inkscape"

svg :: Name -> Name
svg = _nameNamespace ?~ "http://www.w3.org/2000/svg"

labelAttr :: Name
labelAttr = inInkscapeNs "label"

layerLabel :: Lens' Layer (Maybe LayerLabel)
layerLabel = attrs . at labelAttr

allLayers :: Document -> [LayerLabel]
allLayers doc = catMaybes $ doc ^.. traverseLayers . layerLabel

traverseLayers :: Traversal' Document Layer
traverseLayers =
    root
    . deep (filtered (views name (== svg "g")))
    . isLayer

isLayer :: Traversal' Element Layer
isLayer = attributeIs (inInkscapeNs "groupmode") "layer"

byId :: ElementId -> Traversal' Document Element
byId i = root . deep (attributeIs "id" i)

byLabel :: LayerLabel -> Traversal' Document Element
byLabel label =
    root . deep (attributeIs labelAttr label)

setVisible :: Bool -> Traversal' a Element -> a -> a
setVisible vis xs = xs . style "display" ?~ val
  where val = if vis then "inline" else "none"

setOpacity :: Opacity -> Traversal' a Element -> a -> a
setOpacity 0 xs = setVisible False xs
setOpacity o xs = setVisible True xs
                . (xs . opacity .~ o)

opacity :: Lens' Element Opacity
opacity = style "opacity" . iso to from
  where
    to Nothing  = 1
    to (Just x) = maybe 1 id $ readMay $ T.unpack x
    from 1 = Nothing
    from x = Just $ T.pack $ show x

type StyleAttr = Text

style :: StyleAttr -> Lens' Element (Maybe Text)
style s = style' . at s

style' :: Lens' Element (M.Map StyleAttr Text)
style' = attribute "style" . non T.empty . styleString

styleString :: Iso' Text (M.Map StyleAttr Text)
styleString = iso to from
  where
    splitKeyValue x = case T.splitOn ":" x of
                        [k,v] -> M.singleton k v
                        _     -> M.empty
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

readSvg :: MonadIO m => FilePath -> ExceptT String m Svg
readSvg inFile = liftIO $ Xml.readFile def inFile

writeSvg :: MonadIO m => FilePath -> Svg -> ExceptT String m ()
writeSvg outFile = liftIO . Xml.writeFile def outFile
