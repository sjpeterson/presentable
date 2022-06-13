{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Presentable.UI.Brick
    ( runBrick
    ) where

import Control.Monad.IO.Class ( liftIO )
import Data.Bifunctor ( second )

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
import Graphics.Vty.Attributes ( Attr, defAttr )
import Lens.Micro ( (&), (.~), (^.) )

import Presentable.App.Env
    ( AppEnv ( AppEnv, maxDimensions, slideshow, styles ) )
import Presentable.App.State ( AppState
                             , appStatePosition
                             , appStateRect
                             , appStateSlidesBuffer
                             , initState
                             , makeBuffer
                             )
import Presentable.Data.Buffer ( Buffer ( Buffer ), forwardUntil, next, prev )
import Presentable.Data.Config
    ( Color ( Black, Red, Green, Yellow, Blue, Magenta, Cyan, White )
    , Style ( Style )
    , Styles ( Styles, _titleStyle, _subtitleStyle, _errorStyle, _bulletStyle )
    )
import Presentable.Data.Geometry ( Rect ( Rect ), limit )
import Presentable.UI.Brick.Draw ( Name, drawUI )
import Presentable.UI.Brick.Attributes ( bulletAttr
                                       , errorAttr
                                       , subtitleAttr
                                       , titleAttr
                                       )

-- | run the Brick application
runBrick :: AppEnv -> IO ()
runBrick appEnv = defaultMain (app appEnv) (initState appEnv) >> return ()

-- | Create a Brick application from the environment.
app :: AppEnv -> App AppState e Name
app appEnv = App { appDraw = drawUI appEnv
                 , appChooseCursor = neverShowCursor
                 , appHandleEvent = handleEvent appEnv
                 , appStartEvent = appStart appEnv
                 , appAttrMap = const $ attributeMap $ styles appEnv
                 }

-- | Brick application start event. Gets terminal size and processes
-- slideshow accordingly at startup.
appStart :: AppEnv -> AppState -> EventM Name AppState
appStart appEnv appState = do
    vtyOutput <- V.outputIface <$> getVtyHandle
    rect <- liftIO (slideshowRect appEnv <$> V.displayBounds vtyOutput)
    return $ appState & appStatePosition .~ 0
                      & appStateRect .~ rect
                      & appStateSlidesBuffer .~ (makeBuffer appEnv rect)

-- | Brick application event handler.
handleEvent :: AppEnv
            -> AppState
            -> BrickEvent Name e
            -> EventM Name (Next AppState)
handleEvent appEnv appState event = case event of
    (VtyEvent (V.EvKey V.KEsc        [])) -> halt appState
    (VtyEvent (V.EvKey V.KRight      [])) -> continue nextSlide
    (VtyEvent (V.EvKey (V.KChar 'l') [])) -> continue nextSlide
    (VtyEvent (V.EvKey (V.KChar ' ') [])) -> continue nextSlide
    (VtyEvent (V.EvKey V.KLeft       [])) -> continue prevSlide
    (VtyEvent (V.EvKey (V.KChar 'h') [])) -> continue prevSlide
    (VtyEvent (V.EvKey V.KBS         [])) -> continue prevSlide
    (VtyEvent (V.EvResize columns rows)) -> continue $
        fitSlides (slideshowRect appEnv (columns, rows))
    _                                    -> continue appState
  where
    nextSlide = moveBy next
    prevSlide = moveBy prev
    moveBy f = case appState ^. appStateSlidesBuffer of
        Left _       -> appState
        Right buffer ->
            let newBuffer@(Buffer (_, position) _ _) = f buffer
            in appState & appStatePosition .~ position
                        & appStateSlidesBuffer .~ Right newBuffer

    fitSlides rect =
        let position = appState ^. appStatePosition
            atPosition = (> position) . snd
            newBuffer = fmap (forwardUntil atPosition) (makeBuffer appEnv rect)
        in appState & appStateRect .~ rect
                    & appStateSlidesBuffer .~ newBuffer

-- | Compute the slideshow rectangle for some screen size.
slideshowRect :: AppEnv -> (Int, Int) -> Rect
slideshowRect AppEnv {..} (columns, rows) =
    limit maxDimensions $ Rect (columns - 2) (rows - 2)

-- | Convert applications styles to a brick application attribute map.
attributeMap :: Styles -> AttrMap
attributeMap Styles {..} = attrMap V.defAttr $ map (second brickAttribute)
    [ (titleAttr, _titleStyle)
    , (subtitleAttr, _subtitleStyle)
    , (bulletAttr, _bulletStyle)
    , (errorAttr, _errorStyle)
    ]

-- | Convert an application style type to a brick attribute.
brickAttribute :: Style -> Attr
brickAttribute (Style color bold italic) =
    foldl V.withStyle (baseAttr color) variants
  where
    variants = map fst $ filter snd $ [ (V.bold, bold)
                                      , (V.italic, italic)
                                      ]

-- | Get the base attribute for a style based on the color.
baseAttr :: Maybe Color -> Attr
baseAttr (Just color) = fg $ case color of
    Black   -> V.black
    Red     -> V.red
    Green   -> V.green
    Yellow  -> V.yellow
    Blue    -> V.blue
    Magenta -> V.magenta
    Cyan    -> V.cyan
    White   -> V.white
baseAttr Nothing = defAttr

