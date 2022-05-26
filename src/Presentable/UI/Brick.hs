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
    , vBox
    , vLimit
    , withAttr
    )
import Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Lens.Micro ( (^.) )

import Presentable.App.Env ( AppEnv ( slideshow ) )
import Presentable.App.State ( AppState
                             , appStateSlidesBuffer
                             , initState
                             , nextSlide
                             , prevSlide
                             )
import Presentable.Data.Buffer ( bufferCurrent )
import Presentable.Data.Slideshow ( Slide ( TitleSlide, SingleContentSlide )
                                  , SlideContent ( BulletList, NoContent )
                                  , Slideshow ( slideshowCopyright )
                                  )

type Name = ()

app :: AppEnv -> App AppState e Name
app appEnv = App { appDraw = drawUI appEnv
                 , appChooseCursor = neverShowCursor
                 , appHandleEvent = handleEvent
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

drawContent :: SlideContent -> Widget Name
drawContent NoContent          = emptyWidget
drawContent (BulletList items) = padRight Max $
    vBox $ map drawBulletListItem items

drawBulletListItem :: Text -> Widget Name
drawBulletListItem = (<+>) (withAttr bulletAttr (txt "• ")) . txt

handleEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleEvent s (VtyEvent (V.EvKey V.KEsc   [])) = halt s
handleEvent s (VtyEvent (V.EvKey V.KRight [])) = continue $ nextSlide s
handleEvent s (VtyEvent (V.EvKey V.KLeft  [])) = continue $ prevSlide s
handleEvent s _                                = continue $ s

attributeMap :: AttrMap
attributeMap = attrMap V.defAttr
    [ (titleAttr, fg V.yellow `V.withStyle` V.bold)
    , (bulletAttr, fg V.yellow `V.withStyle` V.bold)
    ]

titleAttr, bulletAttr :: AttrName
titleAttr = "titleAttr"
bulletAttr = "bulletAttr"
