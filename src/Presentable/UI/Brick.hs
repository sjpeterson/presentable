{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Presentable.UI.Brick
    ( runBrick
    ) where

import Control.Monad.IO.Class ( liftIO )
import Data.Maybe ( isJust )
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

import Presentable.App.Env ( AppEnv ( AppEnv, maxDimensions, slideshow ) )
import Presentable.App.State ( AppState
                             , appStateRect
                             , appStateSlidesBuffer
                             , initState
                             )
import Presentable.Data.Buffer ( Buffer, bufferOf, next, prev )
import Presentable.Data.Geometry ( Rect ( Rect ), limit )
import Presentable.Data.Slideshow ( Slide
                                  , Slideshow ( slideshowCopyright
                                              , slideshowSlides
                                              )
                                  )
import Presentable.Process.Slideshow ( fitTo, zipValues )
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
    rect <- liftIO (slideshowRect appEnv <$> V.displayBounds vtyOutput)
    return $ appState & appStateRect .~ rect
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
        fitSlides (slideshowRect appEnv (columns, rows))
    _                                    -> continue appState
  where
    nextSlide = over appStateSlidesBuffer (fmap next) appState
    prevSlide = over appStateSlidesBuffer (fmap prev) appState
    fitSlides rect =
        appState & appStateRect .~ rect
                 & appStateSlidesBuffer .~ (makeBuffer appEnv rect)

-- | Compute the slideshow rectangle for some screen size
slideshowRect :: AppEnv -> (Int, Int) -> Rect
slideshowRect AppEnv {..} (columns, rows) =
    limit maxDimensions $ Rect (columns - 2) (rows - 2)

-- | Brick application attribute map
attributeMap :: AttrMap
attributeMap = attrMap V.defAttr
    [ (titleAttr, fg V.yellow `V.withStyle` V.bold)
    , (bulletAttr, fg V.yellow `V.withStyle` V.bold)
    , (errorAttr, fg V.red `V.withStyle` V.bold)
    ]

-- | Make a buffer of slides fitting the given rectangle.
makeBuffer :: AppEnv -> Rect -> Either Text (Buffer (Slide, Int))
makeBuffer AppEnv {..} rect = fmap (bufferOf . zipValues) $
    fitTo rect footerHeight $ slideshowSlides $ slideshow
  where
    footerHeight = if isJust $ slideshowCopyright slideshow then 2 else 0

