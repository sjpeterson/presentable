{-# LANGUAGE OverloadedStrings #-}

module Presentable.UI.Brick.Draw where


import Data.List ( intersperse )
import Data.List.NonEmpty ( NonEmpty )
import qualified Data.List.NonEmpty as NE
import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import Brick
    ( Padding ( Max, Pad )
    , Widget
    , (<=>)
    , (<+>)
    , emptyWidget
    , hLimit
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
import Lens.Micro ( (^.) )

import Presentable.App.Env ( AppEnv ( slideshow ) )
import Presentable.App.State ( AppState
                             , appStateRect
                             , appStateSlidesBuffer
                             )
import Presentable.Data.Buffer ( bufferCurrent )
import Presentable.Data.Geometry ( Rect ( Rect ) )
import Presentable.Data.Slideshow
    ( BulletList ( BulletList, unBulletList )
    , BulletListItem ( BulletListItem )
    , Slide ( SingleContentSlide , TitleSlide )
    , SlideContent ( BulletListContent, NoContent, PlainTextContent )
    , Slideshow ( slideshowCopyright )
    )
import Presentable.Data.TextBlock ( InlineTextTag ( PlainText )
                                  , TaggedText
                                  , TextBlock ( unTextBlock )
                                  )
import Presentable.Data.Wrappable ( wrapRelaxedAt )
import Presentable.UI.Brick.Attributes
    ( bulletAttr
    , copyrightAttr
    , errorAttr
    , slideTitleAttr
    , subtitleAttr
    , titleAttr
    )

type Name = ()

-- | Draw the application state.
drawUI :: AppEnv -> AppState -> [Widget Name]
drawUI appEnv appState =
    [ C.center $ (padAll 1) $ hLimit columns $ vLimit rows $ inner ]
  where
    inner = vBox [ padBottom Max $ padRight Max $ slide
                 , copyrightNotice
                 ]
    slide = either drawError (drawSlide columns) $
        fst . bufferCurrent <$> appState ^. appStateSlidesBuffer
    Rect columns rows = appState ^. appStateRect
    copyrightNotice = case (slideshowCopyright $ slideshow appEnv) of
        Nothing        -> emptyWidget
        Just copyright ->
            padLeft Max $ withAttr copyrightAttr $ str $ show copyright

-- | Draw an error.
drawError :: Text -> Widget Name
drawError = C.hCenter . withAttr errorAttr . txt

-- | Draw a slide to the given width.
drawSlide :: Int -> Slide -> Widget Name
drawSlide _ (TitleSlide title subtitle) = case subtitle of
    Nothing -> titleWidget
    Just s  -> C.center $ vBox [ padBottom (Pad 1) titleWidget
                               , C.hCenter $ withAttr subtitleAttr $ txt s
                               ]
  where
    titleWidget = C.hCenter $ withAttr titleAttr $ txt title
drawSlide columns (SingleContentSlide title content) = padBottom Max $
    vBox [ padBottom (Pad 1) $ withAttr slideTitleAttr $ txt title
         , drawContent columns content
         ]

-- | Draw slide contents to the given width.
drawContent :: Int -> SlideContent -> Widget Name
drawContent _       NoContent                              = emptyWidget
drawContent columns (BulletListContent (BulletList items)) = padRight Max $
    vBox $ map (drawBulletListItem columns topLevelMarker) (NE.toList items)
drawContent columns (PlainTextContent paragraphs)          = padRight Max $
    vBox $ intersperse (txt " ") $
        map (drawTextBlock columns) (NE.toList paragraphs)

-- | Draw a bullet list item to the given width.
drawBulletListItem :: Int -> BulletMarker -> BulletListItem -> Widget Name
drawBulletListItem columns marker (BulletListItem tb sublist) =
    withAttr bulletAttr (drawMarker marker) <+>
        (drawTextBlock (columns - 2) tb <=>
        fromMaybe emptyWidget (fmap drawSublist sublist))
  where
    drawSublist =
        vBox . map drawItemNextLevel . NE.toList . unBulletList
    drawItemNextLevel = drawBulletListItem (columns -2) (nextMarker marker)

-- | Draw a text block wrapped at the specified width.
drawTextBlock :: Int -> TextBlock -> Widget Name
drawTextBlock columns tb =
    vBox $ map (drawLine . unTextBlock) $ NE.toList $ wrapRelaxedAt columns tb

-- | Draw a single line of text.
drawLine :: NonEmpty TaggedText -> Widget Name
drawLine line = drawTaggedText t <+> case ts of
    Nothing    -> emptyWidget
    (Just ts') -> txt " " <+> drawLine ts'
  where
    (t, ts) = NE.uncons line

-- | Draw a single tagged text element.
drawTaggedText :: TaggedText -> Widget Name
drawTaggedText (s, PlainText) = txt s

-- | Bullet markers
data BulletMarker = SolidCircle
                  | OutlinedCircle
                  | Square
                  | Hyphen

-- | The top-level bullet marker
topLevelMarker :: BulletMarker
topLevelMarker = SolidCircle

-- | Cycle through bullet markers.
nextMarker :: BulletMarker -> BulletMarker
nextMarker SolidCircle    = OutlinedCircle
nextMarker OutlinedCircle = Square
nextMarker Square         = Hyphen
nextMarker _              = SolidCircle

-- | Draw a bullet marker.
drawMarker :: BulletMarker -> Widget Name
drawMarker marker = txt $ case marker of
    SolidCircle    -> "• "
    OutlinedCircle -> "◦ "
    Square         -> "▪ "
    Hyphen         -> "⁃ "
