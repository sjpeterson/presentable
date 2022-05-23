{-# LANGUAGE RecordWildCards #-}

-- |
-- Module
-- Copyright
-- Author
--
-- Application state for Presentable

module Presentable.App.State where

import Presentable.Data.Presentation
    ( Presentation ( Presentation, presentationSlides )
    , Slide
    )

data AppState = AppState
    { appStatePresentationBuffer :: PresentationBuffer
    } deriving ( Eq, Show)

data PresentationBuffer = PresentationBuffer
    { presentationBufferCurrent :: Slide
    , presentationBufferNext :: [Slide]
    , presentationBufferPrevious :: [Slide]
    } deriving ( Eq, Show )

initState :: Presentation -> AppState
initState Presentation {..} =
    AppState $ PresentationBuffer titleSlide nextSlides []
  where
    titleSlide:nextSlides = presentationSlides
    
