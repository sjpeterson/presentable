{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Presentable.App.State where

import Lens.Micro ( Lens', lens, over )

import Presentable.App.Env ( AppEnv ( AppEnv, slideshow ) )
import Presentable.Data.Buffer ( Buffer ( Buffer ), bufferOf, next, prev )
import Presentable.Data.Slideshow
    ( Slideshow ( Slideshow, slideshowSlides )
    , Slide
    )

-- | Application state type.
data AppState = AppState
    { _appStateSlidesBuffer :: Buffer Slide
    } deriving ( Eq, Show )

-- | A lens for the slides buffer.
appStateSlidesBuffer :: Lens' AppState (Buffer Slide)
appStateSlidesBuffer = lens
    _appStateSlidesBuffer
    (\appState b -> appState { _appStateSlidesBuffer = b })

-- | Create the initial state from the environment.
initState :: AppEnv -> AppState
initState AppEnv {..} = AppState {..}
  where
    _appStateSlidesBuffer = bufferOf $ slideshowSlides slideshow
