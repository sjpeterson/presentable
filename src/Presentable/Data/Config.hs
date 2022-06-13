{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Presentable.Data.Config where

import Data.Aeson.TH ( deriveFromJSON )
import Data.Aeson.Types ( FromJSON ( parseJSON ), withText )
import Data.Maybe ( fromMaybe )
import qualified Data.Text as T

import Lens.Micro ( (&), (%~) )
import Lens.Micro.TH ( makeLenses )

import Presentable.Data.AesonOptions ( stripPrefix )
import Presentable.Data.Geometry ( Rect ( Rect, rectColumns, rectRows ) )

-- | Valid terminal colors.
data Color = Black
           | Red
           | Green
           | Yellow
           | Blue
           | Magenta
           | Cyan
           | White
           deriving ( Eq, Show )

instance FromJSON Color where
  parseJSON = withText "Color" $ \s -> case T.toLower s of
    "red"     -> return Red
    "green"   -> return Green
    "yellow"  -> return Yellow
    "blue"    -> return Blue
    "magenta" -> return Magenta
    "cyan"    -> return Cyan
    "black"   -> return Black
    "white"   -> return White
    _         -> fail $ unwords [T.unpack s, "is not a valid color"]

-- | Text style data type.
data Style = Style
    { _styleColor :: Maybe Color
    , _styleBold :: Bool
    , _styleItalic :: Bool
    } deriving ( Eq, Show )

makeLenses ''Style

-- | Complete styles data type.
data Styles = Styles
    { _bulletStyle :: Style
    , _copyrightStyle :: Style
    , _errorStyle :: Style
    , _slideTitleStyle :: Style
    , _subtitleStyle :: Style
    , _titleStyle :: Style
    } deriving ( Eq, Show )

makeLenses ''Styles

-- | Complete configuration.
data Config = Config
    { _configMaxDimensions :: Rect
    , _configStyles :: Styles
    } deriving ( Eq, Show )

makeLenses ''Config

-- | Config file draw type.
data PartialDrawConfig = PartialDrawConfig
    { partialDrawConfigMaxColumns :: Maybe Int
    , partialDrawConfigMaxRows :: Maybe Int
    } deriving ( Eq, Show )

$(deriveFromJSON (stripPrefix 17) ''PartialDrawConfig)

-- | Config file style type.
data PartialStyle = PartialStyle
    { partialStyleColor :: Maybe Color
    , partialStyleBold :: Maybe Bool
    , partialStyleItalic :: Maybe Bool
    } deriving ( Eq, Show )

$(deriveFromJSON (stripPrefix 12) ''PartialStyle)

-- | Config file styles type.
data PartialStyles = PartialStyles
    { partialStylesBullet :: Maybe PartialStyle
    , partialStylesCopyright :: Maybe PartialStyle
    , partialStylesError :: Maybe PartialStyle
    , partialStylesSlideTitle :: Maybe PartialStyle
    , partialStylesSubtitle :: Maybe PartialStyle
    , partialStylesTitle :: Maybe PartialStyle
    } deriving ( Eq, Show )

$(deriveFromJSON (stripPrefix 13) ''PartialStyles)

-- | Partial config data type.
data PartialConfig = PartialConfig
    { partialConfigDraw :: Maybe PartialDrawConfig
    , partialConfigStyles :: Maybe PartialStyles
    } deriving ( Eq, Show )

$(deriveFromJSON (stripPrefix 13) ''PartialConfig)

-- | Overload a config with a partial config.
overloadedWith :: Config -> PartialConfig -> Config
overloadedWith baseConfig PartialConfig {..} =
    baseConfig & configMaxDimensions %~ overloadDimensions
               & configStyles %~ overloadStyles
  where
    overloadDimensions rect = case partialDimensions of
        (Just maxColumns, Just maxRows) -> Rect maxColumns maxRows
        (Just maxColumns, Nothing)      -> rect { rectColumns = maxColumns }
        (Nothing, Just maxRows)         -> rect { rectRows = maxRows }
        (Nothing, Nothing)              -> rect
    partialDimensions = case partialConfigDraw of
        Nothing -> (Nothing, Nothing)
        Just c  -> (partialDrawConfigMaxColumns c, partialDrawConfigMaxRows c)

    overloadStyles = case partialConfigStyles of
        Nothing -> id
        Just PartialStyles {..} -> \s ->
            s & bulletStyle %~ overloadWith partialStylesBullet
              & copyrightStyle %~ overloadWith partialStylesCopyright
              & errorStyle %~ overloadWith partialStylesError
              & slideTitleStyle %~ overloadWith partialStylesSlideTitle
              & subtitleStyle %~ overloadWith partialStylesSubtitle
              & titleStyle %~ overloadWith partialStylesTitle

    overloadWith Nothing = id
    overloadWith (Just PartialStyle {..}) = \s ->
        s & styleColor %~ (\c -> case partialStyleColor of
                                     Nothing -> c
                                     Just c' -> Just c')
          & styleBold %~ (flip fromMaybe) partialStyleBold
          & styleItalic %~ (flip fromMaybe) partialStyleItalic
