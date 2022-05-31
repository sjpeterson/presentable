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

import Control.Monad.IO.Class ( liftIO )
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
    , BrickEvent ( VtyEvent )
    , EventM
    , Next
    , attrMap
    , continue
    , defaultMain
    , fg
    , getVtyHandle
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
import Presentable.Data.Buffer ( Buffer, bufferOf, next, prev )
import Presentable.Data.Geometry ( Rect ( Rect, rectColumns ) )
import Presentable.Data.Slideshow ( Slide, Slideshow ( slideshowSlides ) )
import Presentable.Process.Slideshow ( fitTo )
import Presentable.UI.Brick.Draw ( Name, drawUI )
import Presentable.UI.Brick.Attributes ( bulletAttr, errorAttr, titleAttr )

-- | run the Brick application
runBrick :: AppEnv -> IO ()
runBrick appEnv = defaultMain (app appEnv) (initState appEnv) >> return ()

-- | Create a Brick application from the environment.
app :: AppEnv -> App AppState e Name
app appEnv = App { appDraw = drawUI appEnv
                 , appChooseCursor = neverShowCursor
                 , appHandleEvent = handleEvent appEnv
                 , appStartEvent = appStart appEnv
                 , appAttrMap = const attributeMap
                 }

-- | Brick application start event. Gets terminal size and processes
-- slideshow accordingly at startup.
appStart :: AppEnv -> AppState -> EventM Name AppState
appStart appEnv appState = do
    vtyOutput <- V.outputIface <$> getVtyHandle
    (columns, rows) <- liftIO (V.displayBounds vtyOutput)
    let rect = Rect (columns - 2) (rows - 2)
    return $ appState & appStateColumns .~ rectColumns rect
                      & appStateSlidesBuffer .~ (makeBuffer appEnv rect)

-- | Brick application event handler.
handleEvent :: AppEnv
            -> AppState
            -> BrickEvent Name e
            -> EventM Name (Next AppState)
handleEvent appEnv appState event = case event of
    (VtyEvent (V.EvKey V.KEsc   []))     -> halt appState
    (VtyEvent (V.EvKey V.KRight []))     -> continue nextSlide
    (VtyEvent (V.EvKey V.KLeft  []))     -> continue prevSlide
    (VtyEvent (V.EvResize columns rows)) -> continue $
        fitSlides (Rect (columns - 2) (rows - 2))
    _                                    -> continue appState
  where
    nextSlide = over appStateSlidesBuffer (fmap next) appState
    prevSlide = over appStateSlidesBuffer (fmap prev) appState
    fitSlides rect@(Rect {..}) =
        appState & appStateColumns .~ rectColumns
                 & appStateSlidesBuffer .~ (makeBuffer appEnv rect)

-- | Brick application attribute map
attributeMap :: AttrMap
attributeMap = attrMap V.defAttr
    [ (titleAttr, fg V.yellow `V.withStyle` V.bold)
    , (bulletAttr, fg V.yellow `V.withStyle` V.bold)
    , (errorAttr, fg V.red `V.withStyle` V.bold)
    ]

-- | Make a buffer of slides fitting the given rectangle.
makeBuffer :: AppEnv -> Rect -> Either Text (Buffer Slide)
makeBuffer appEnv rect =
    (fmap bufferOf) $ fitTo rect $ slideshowSlides $ slideshow appEnv
