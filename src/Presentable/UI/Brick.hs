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

import Brick
    ( App ( App
          , appDraw
          , appChooseCursor
          , appHandleEvent
          , appStartEvent
          , appAttrMap
          )
    , AttrMap
    , BrickEvent ( VtyEvent )
    , EventM
    , Next
    , attrMap
    , continue
    , defaultMain
    , fg
    , halt
    , neverShowCursor
    )
import qualified Graphics.Vty as V
import Lens.Micro ( (&), (.~), over )

import Presentable.App.Env ( AppEnv ( slideshow ) )
import Presentable.App.State ( AppState
                             , appStateColumns
                             , appStateSlidesBuffer
                             , initState
                             )
import Presentable.Data.Buffer ( bufferOf, next, prev )
import Presentable.Data.Geometry ( Rect ( Rect, rectColumns ) )
import Presentable.Data.Slideshow ( Slideshow ( slideshowSlides ) )
import Presentable.Process.Slideshow ( fitTo )
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
