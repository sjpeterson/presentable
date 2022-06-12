{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Presentable.Data.Config where

import Data.Aeson.TH ( deriveFromJSON )
import Data.Aeson.Types ( FromJSON ( parseJSON ), withText )
import Data.Maybe ( fromMaybe )
import qualified Data.Text as T

import Lens.Micro ( Lens', (&), (%~), lens )

import Presentable.Data.AesonOptions ( stripPrefix )
import Presentable.Data.Geometry ( Rect ( Rect, rectColumns, rectRows ) )

-- | Complete configuration.
data Config = Config
    { _configMaxDimensions :: Rect
    , _configStyles :: Styles
    } deriving ( Eq, Show )

-- | A lens for the maximum dimensions.
configMaxDimensions :: Lens' Config Rect
configMaxDimensions = lens
    _configMaxDimensions
    (\config b -> config { _configMaxDimensions = b })

-- | A lens for the styles.
configStyles :: Lens' Config Styles
configStyles = lens
    _configStyles
    (\config b -> config { _configStyles = b })

-- | Complete styles data type.
data Styles = Styles
    { _titleStyle :: Style
    , _subtitleStyle :: Style
    , _errorStyle :: Style
    , _bulletStyle :: Style
    } deriving ( Eq, Show )

-- | A lens for the bullet style
bulletStyle :: Lens' Styles Style
bulletStyle = lens _bulletStyle (\styles s -> styles { _bulletStyle = s })

-- | A lens for the error style
errorStyle :: Lens' Styles Style
errorStyle = lens _errorStyle (\styles s -> styles { _errorStyle = s })

-- | A lens for the subtitle style
subtitleStyle :: Lens' Styles Style
subtitleStyle = lens _subtitleStyle (\styles s -> styles { _subtitleStyle = s })

-- | A lens for the title style
titleStyle :: Lens' Styles Style
titleStyle = lens _titleStyle (\styles s -> styles { _titleStyle = s })

-- | Text style data type.
data Style = Style
    { _styleColor :: Maybe Color
    , _styleBold :: Bool
    , _styleItalic :: Bool
    } deriving ( Eq, Show )

-- | A lens for the style color.
styleColor :: Lens' Style (Maybe Color)
styleColor = lens _styleColor (\style c -> style { _styleColor = c })

-- | A lens for the style bold flag.
styleBold :: Lens' Style Bool
styleBold = lens _styleBold (\style b -> style { _styleBold = b })

-- | A lens for the style italic flag.
styleItalic :: Lens' Style Bool
styleItalic = lens _styleItalic (\style b -> style { _styleItalic = b })

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
    { partialStylesTitle :: Maybe PartialStyle
    , partialStylesSubtitle :: Maybe PartialStyle
    , partialStylesError :: Maybe PartialStyle
    , partialStylesBullet :: Maybe PartialStyle
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
              & errorStyle %~ overloadWith partialStylesError
              & subtitleStyle %~ overloadWith partialStylesSubtitle
              & titleStyle %~ overloadWith partialStylesTitle

    overloadWith Nothing = id
    overloadWith (Just PartialStyle {..}) = \s ->
        s & styleColor %~ (\c -> case partialStyleColor of
                                     Nothing -> c
                                     Just c' -> Just c')
          & styleBold %~ (flip fromMaybe) partialStyleBold
          & styleItalic %~ (flip fromMaybe) partialStyleItalic
