{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Presentable.App.State where

import Data.Maybe ( isJust )
import Data.Text ( Text )

import Lens.Micro.TH ( makeLenses )

import Presentable.App.Env ( AppEnv ( AppEnv, slideshow ) )
import Presentable.Data.Buffer ( Buffer, bufferOf )
import Presentable.Data.Geometry ( Rect ( Rect ) )
import Presentable.Data.Slideshow ( Slide, slideshowSlides, slideshowCopyright )
import Presentable.Process.Slideshow ( fitTo, zipValues )

-- | Type alias for the slides buffer. The slides buffer is a buffer of slides
-- and their positional value (used to remain in place when re-fitting the
-- the slideshow to a new window size).
type SlidesBuffer = Buffer (Slide, Int)

-- | Application state type. Holds all data required to draw the UI.
data AppState = AppState
    { _appStatePosition :: Int
    , _appStateSlidesBuffer :: Either Text SlidesBuffer
    , _appStateRect :: Rect
    } deriving ( Eq, Show )

makeLenses ''AppState

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

