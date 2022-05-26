module Presentable.Process.Slideshow where

import Data.Foldable ( foldr' )
import Data.List.NonEmpty ( NonEmpty ( (:|) ), (<|) )
import qualified Data.List.NonEmpty as NE ( init, last )

import Presentable.Data.Buffer ( Buffer )
import Presentable.Data.Geometry ( Rect )
import Presentable.Data.Slideshow ( Slideshow, Slide )

fitTo :: Rect -> NonEmpty Slide -> NonEmpty Slide
fitTo rect slides = foldr' f lastSlides (NE.init slides)
  where
    f = (<>) . fitOneTo rect
    lastSlides = fitOneTo rect $ NE.last slides

fitOneTo :: Rect -> Slide -> NonEmpty Slide
fitOneTo rect slide = slide :| []

heightAtWidth :: Int -> Slide -> Int
heightAtWidth = undefined