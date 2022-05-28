{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Module
-- Copyright
-- License
--
-- Brick

module Presentable.UI.Brick
    ( runBrick
    ) where

import Control.Monad.Reader ( asks, liftIO )
import Data.List.NonEmpty ( NonEmpty ( (:|) ) )
import qualified Data.List.NonEmpty as NE
import Data.Text ( Text )

import Brick
    ( App ( App
          , appDraw
          , appChooseCursor
          , appHandleEvent
          , appStartEvent
          , appAttrMap
          )
    , AttrMap
    , AttrName
    , BrickEvent ( VtyEvent )
    , EventM
    , Next
    , Padding ( Max, Pad )
    , Widget
    , (<+>)
    , attrMap
    , attrName
    , continue
    , defaultMain
    , emptyWidget
    , fg
    , halt
    , hLimit
    , neverShowCursor
    , padAll
    , padBottom
    , padLeft
    , padRight
    , str
    , txt
    , txtWrap
    , vBox
    , vLimit
    , withAttr
    )
import Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Lens.Micro ( (&), (.~), (^.), over, set )

import Presentable.App.Env ( AppEnv ( slideshow ) )
import Presentable.App.State ( AppState
                             , appStateColumns
                             , appStateSlidesBuffer
                             , initState
                             )
import Presentable.Data.Buffer ( bufferCurrent, bufferOf, next, prev )
import Presentable.Data.Geometry ( Rect ( Rect, rectColumns ) )
import Presentable.Data.Slideshow ( InlineTextTag ( PlainText )
                                  , Slide ( SingleContentSlide
                                          , TitleSlide
                                          )
                                  , SlideContent ( BulletList, NoContent )
                                  , Slideshow ( slideshowCopyright
                                              , slideshowSlides
                                              )
                                  , TaggedText
                                  , TextBlock ( TextBlock )
                                  )
import Presentable.Process.Slideshow ( fitTo, wrapRelaxedAt )
import Presentable.UI.Brick.Draw ( Name, drawUI )
import Presentable.UI.Brick.Attributes ( bulletAttr, errorAttr, titleAttr )

app :: AppEnv -> App AppState e Name
app appEnv = App { appDraw = drawUI appEnv
                 , appChooseCursor = neverShowCursor
                 , appHandleEvent = handleEvent appEnv
                 , appStartEvent = return
                 , appAttrMap = const attributeMap
                 }

runBrick :: AppEnv -> IO ()
runBrick appEnv = defaultMain (app appEnv) (initState appEnv) >> return ()

handleEvent :: AppEnv
            -> AppState
            -> BrickEvent Name e
            -> EventM Name (Next AppState)
handleEvent appEnv appState event = case event of
    (VtyEvent (V.EvKey V.KEsc   []))     -> halt appState
    (VtyEvent (V.EvKey V.KRight []))     -> continue nextSlide
    (VtyEvent (V.EvKey V.KLeft  []))     -> continue prevSlide
    (VtyEvent (V.EvResize columns rows)) -> continue $
        fitSlides (Rect columns rows)
    _                                    -> continue appState
  where
    nextSlide = over appStateSlidesBuffer (fmap next) appState
    prevSlide = over appStateSlidesBuffer (fmap prev) appState
    fitSlides rect@(Rect {..}) =
        appState & appStateColumns .~ rectColumns
                 & appStateSlidesBuffer .~ ((fmap bufferOf) $ fitTo rect $ slideshowSlides $ slideshow appEnv)

attributeMap :: AttrMap
attributeMap = attrMap V.defAttr
    [ (titleAttr, fg V.yellow `V.withStyle` V.bold)
    , (bulletAttr, fg V.yellow `V.withStyle` V.bold)
    , (errorAttr, fg V.red `V.withStyle` V.bold)
    ]
