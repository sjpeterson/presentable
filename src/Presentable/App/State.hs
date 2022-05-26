{-# LANGUAGE RecordWildCards #-}

-- |
-- Module     : Presentable.App.State
-- Copyright  : 2022 Stefan Peterson
-- License    : MIT
--
-- Application state for Presentable

module Presentable.App.State where

import Lens.Micro ( Lens', lens, over )

import Presentable.App.Env ( AppEnv ( AppEnv, slideshow ) )
import Presentable.Data.Buffer ( Buffer ( Buffer ), next, prev )
import Presentable.Data.Slideshow
    ( Slideshow ( Slideshow, slideshowSlides )
    , Slide
    )

data AppState = AppState
    { _appStateSlidesBuffer :: Buffer Slide
    } deriving ( Eq, Show )

appStateSlidesBuffer :: Lens' AppState (Buffer Slide)
appStateSlidesBuffer = lens
    _appStateSlidesBuffer
    (\appState b -> appState { _appStateSlidesBuffer = b })

initState :: AppEnv -> AppState
initState AppEnv {..} =
    AppState $ Buffer titleSlide nextSlides []
  where
    titleSlide:nextSlides = slideshowSlides slideshow

nextSlide :: AppState -> AppState
nextSlide = over appStateSlidesBuffer next

prevSlide :: AppState -> AppState
prevSlide = over appStateSlidesBuffer prev