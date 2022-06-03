{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Presentable.App.State where

import Data.Text ( Text )

import Lens.Micro ( Lens', lens )

import Presentable.App.Env ( AppEnv ( AppEnv, slideshow ) )
import Presentable.Data.Buffer ( Buffer )
import Presentable.Data.Geometry ( Rect ( Rect ) )
import Presentable.Data.Slideshow ( Slide )

-- | Application state type.
data AppState = AppState
    { _appStateSlidesBuffer :: Either Text (Buffer (Slide,  Int))
    , _appStateRect :: Rect
    } deriving ( Eq, Show )

-- | A lens for the slides buffer.
appStateSlidesBuffer :: Lens' AppState (Either Text (Buffer (Slide, Int)))
appStateSlidesBuffer = lens
    _appStateSlidesBuffer
    (\appState b -> appState { _appStateSlidesBuffer = b })

-- | A lens for the slideshow rectangle.
appStateRect :: Lens' AppState Rect
appStateRect = lens
    _appStateRect
    (\appState c -> appState { _appStateRect = c })

-- | Create the initial state from the environment.
initState :: AppEnv -> AppState
initState AppEnv {..} = AppState {..}
  where
    _appStateSlidesBuffer = Left "Not initialized"
    _appStateRect = Rect 0 0
