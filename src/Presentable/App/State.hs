{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Presentable.App.State where

import Data.Text ( Text )

import Lens.Micro ( Lens', lens )

import Presentable.App.Env ( AppEnv ( AppEnv, slideshow ) )
import Presentable.Data.Buffer ( Buffer, bufferOf )
import Presentable.Data.Slideshow ( Slide, Slideshow ( slideshowSlides ) )

-- | Application state type.
data AppState = AppState
    { _appStateSlidesBuffer :: Either Text (Buffer Slide)
    , _appStateColumns :: Int
    } deriving ( Eq, Show )

-- | A lens for the slides buffer.
appStateSlidesBuffer :: Lens' AppState (Either Text (Buffer Slide))
appStateSlidesBuffer = lens
    _appStateSlidesBuffer
    (\appState b -> appState { _appStateSlidesBuffer = b })

-- | A lens for the columns property
appStateColumns :: Lens' AppState Int
appStateColumns = lens
    _appStateColumns
    (\appState c -> appState { _appStateColumns = c })

-- | Create the initial state from the environment.
initState :: AppEnv -> AppState
initState AppEnv {..} = AppState {..}
  where
    _appStateSlidesBuffer = Right $ bufferOf $ slideshowSlides slideshow
    _appStateColumns = 80
