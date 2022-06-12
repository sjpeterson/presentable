{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Presentable.App.State where

import Data.Maybe ( isJust )
import Data.Text ( Text )

import Lens.Micro ( Lens', lens )

import Presentable.App.Env ( AppEnv ( AppEnv, slideshow ) )
import Presentable.Data.Buffer ( Buffer, bufferOf )
import Presentable.Data.Geometry ( Rect ( Rect ) )
import Presentable.Data.Slideshow ( Slide, slideshowSlides, slideshowCopyright )
import Presentable.Process.Slideshow ( fitTo, zipValues )

-- | Application state type.
data AppState = AppState
    { _appStatePosition :: Int
    , _appStateSlidesBuffer :: Either Text (Buffer (Slide,  Int))
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
initState _ = AppState
    { _appStatePosition = 0
    , _appStateSlidesBuffer = Left "Not initialized"
    , _appStateRect = Rect 0 0
    }

-- | Make a buffer of slides fitting the given rectangle.
makeBuffer :: AppEnv -> Rect -> Either Text (Buffer (Slide, Int))
makeBuffer AppEnv {..} rect = fmap (bufferOf . zipValues) $
    fitTo rect footerHeight $ slideshowSlides $ slideshow
  where
    footerHeight = if isJust $ slideshowCopyright slideshow then 2 else 0

