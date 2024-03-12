module Presentable.Config (getConfig) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (decodeFileEither)
import System.Directory (
    XdgDirectory (XdgConfig),
    doesFileExist,
    getXdgDirectory,
 )
import System.FilePath (combine)

import Presentable.Data.Config (
    Config (Config),
    Style (Style),
    Styles (
        Styles,
        _bulletStyle,
        _copyrightStyle,
        _errorStyle,
        _slideTitleStyle,
        _subtitleStyle,
        _titleStyle
    ),
    overloadedWith,
 )
import Presentable.Data.Geometry (Rect (Rect, rectColumns, rectRows))

-- | A type alias for config errors.
type ConfigError = Text

-- | The default config.
defaultConfig :: Config
defaultConfig = Config maxDimensions defaultStyles
  where
    maxDimensions = Rect{rectColumns = 80, rectRows = 22}

-- | Default styles.
defaultStyles :: Styles
defaultStyles =
    Styles
        { _bulletStyle = defaultStyle
        , _copyrightStyle = defaultStyle
        , _errorStyle = defaultStyle
        , _slideTitleStyle = defaultStyle
        , _subtitleStyle = defaultStyle
        , _titleStyle = defaultStyle
        }
  where
    defaultStyle = Style Nothing False False

-- | Read config from file or fall back to default.
getConfig :: IO (Either ConfigError Config)
getConfig = do
    userConfigPath <- getUserConfigPath
    case userConfigPath of
        Nothing -> return $ Right defaultConfig
        Just path -> do
            userConfigDecodeResult <- decodeFileEither path
            return $ case userConfigDecodeResult of
                Left err -> Left $ T.pack $ show err
                Right userConfig ->
                    Right $ defaultConfig `overloadedWith` userConfig

-- | Returns the user config path if it exists.
getUserConfigPath :: IO (Maybe FilePath)
getUserConfigPath = do
    path <-
        flip combine "default.yml"
            <$> getXdgDirectory XdgConfig "presentable"
    fileExists <- doesFileExist path
    return $ if fileExists then Just path else Nothing
