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
    , Widget
    , attrMap
    , attrName
    , continue
    , defaultMain
    , fg
    , halt
    , neverShowCursor
    , str
    , withAttr
    )
import Brick.Widgets.Center as C
import qualified Graphics.Vty as V

import Presentable.App.State ( AppState, initState )
import Presentable.Data.Presentation ( Presentation )

type Name = ()

app :: App AppState e Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const attributeMap
          }

runBrick :: Presentation -> IO ()
runBrick presentation = do
    let initialState = initState presentation
    finalState <- defaultMain app initialState
    -- use finalState and exit
    putStrLn $ show presentation

drawUI :: AppState -> [Widget Name]
drawUI s = [ C.center $ C.hCenter $ withAttr titleAttr $ str "PRESENTABLE" ]

handleEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleEvent s (VtyEvent (V.EvKey V.KEsc [])) = halt s
handleEvent s _                              = continue $ s

attributeMap :: AttrMap
attributeMap = attrMap V.defAttr
    [ (titleAttr, fg V.yellow `V.withStyle` V.bold)
    , (bulletAttr, fg V.yellow )
    ]

titleAttr, bulletAttr :: AttrName
titleAttr = "titleAttr"
bulletAttr = "bulletAttr"
