{-# LANGUAGE OverloadedStrings #-}

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
import Lens.Micro ( (^.), over, set )

import Presentable.App.Env ( AppEnv ( slideshow ) )
import Presentable.App.State ( AppState
                             , appStateSlidesBuffer
                             , initState
                             )
import Presentable.Data.Buffer ( bufferCurrent, bufferOf, next, prev )
import Presentable.Data.Geometry ( Rect ( Rect ) )
import Presentable.Data.Slideshow ( InlineTextTag ( PlainText )
                                  , Slide ( ErrorSlide
                                          , TitleSlide
                                          , SingleContentSlide
                                          )
                                  , SlideContent ( BulletList, NoContent )
                                  , Slideshow ( slideshowCopyright
                                              , slideshowSlides
                                              )
                                  , TaggedText
                                  , TextBlock ( TextBlock )
                                  )
import Presentable.Process.Slideshow ( fitTo )

type Name = ()

app :: AppEnv -> App AppState e Name
app appEnv = App { appDraw = drawUI appEnv
                 , appChooseCursor = neverShowCursor
                 , appHandleEvent = handleEvent appEnv
                 , appStartEvent = return
                 , appAttrMap = const attributeMap
                 }

runBrick :: AppEnv -> IO ()
runBrick appEnv = defaultMain (app appEnv) (initState appEnv) >> return ()

drawUI :: AppEnv -> AppState -> [Widget Name]
drawUI appEnv appState =
    [ C.center $ hLimit 80 $ vLimit 24 $ (padAll 1) inner ]
  where
    inner = vBox [ padBottom Max $ padRight Max $ slide
                 , copyrightNotice
                 ]
    slide = drawSlide $ bufferCurrent $ appState ^. appStateSlidesBuffer
    copyrightNotice = case (slideshowCopyright $ slideshow appEnv) of
        Nothing        -> emptyWidget
        Just copyright -> padLeft Max $ str $ show copyright

drawSlide :: Slide -> Widget Name
drawSlide (TitleSlide title subtitle) = case subtitle of
    Nothing -> titleWidget
    Just s -> C.center $ vBox [ padBottom (Pad 1) titleWidget , C.hCenter $ txt s ]
  where
    titleWidget = C.hCenter $ withAttr titleAttr $ txt title
drawSlide (SingleContentSlide title content) = padBottom Max $
    vBox [ padBottom (Pad 1) $ withAttr titleAttr $ txt title
         , drawContent content
         ]
drawSlide (ErrorSlide message) = withAttr errorAttr $ txtWrap message

drawContent :: SlideContent -> Widget Name
drawContent NoContent          = emptyWidget
drawContent (BulletList items) = padRight Max $
    vBox $ map drawBulletListItem items

drawBulletListItem :: TextBlock -> Widget Name
drawBulletListItem (TextBlock tb) =
    (<+>) (withAttr bulletAttr (txt "• ")) (txtWrap s)
  where
    s = case NE.head tb of
        (s', PlainText) -> s'

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
    nextSlide = over appStateSlidesBuffer next appState
    prevSlide = over appStateSlidesBuffer prev appState
    fitSlides rect = set
        appStateSlidesBuffer
        (bufferOf $ fitTo rect $ slideshowSlides $ slideshow appEnv)
        appState

attributeMap :: AttrMap
attributeMap = attrMap V.defAttr
    [ (titleAttr, fg V.yellow `V.withStyle` V.bold)
    , (bulletAttr, fg V.yellow `V.withStyle` V.bold)
    , (errorAttr, fg V.red `V.withStyle` V.bold)
    ]

titleAttr, bulletAttr, errorAttr :: AttrName
titleAttr = "titleAttr"
bulletAttr = "bulletAttr"
errorAttr = "errorAttr"