{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Presentable.Parse.SlideshowSpec where

import Data.Either ( isLeft )
import qualified Data.Text as T
import Test.Hspec ( Spec, describe, it, shouldBe, shouldSatisfy )

import Presentable.Data.Slideshow ( Copyright (..)
                                  , CopyrightYear (..)
                                  , Slideshow (..)
                                  , Slide (..)
                                  , SlideContent (..)
                                  , plainTextBlock
                                  )
import Presentable.Parse.Slideshow ( parseSlideshow, parseSlide )

spec :: Spec
spec = do
    describe "parseSlide" $ do
        it "parses an empty slide" $ do
            parseSlide "" "\n## Slide Title" `shouldBe`
                Right (SingleContentSlide "Slide Title" NoContent)
        it "parses a bullet list slide" $ do
            parseSlide "" "\n## Slide Title\n\n- First item\n- Second item"
                `shouldBe` Right (SingleContentSlide
                    "Slide Title"
                    (BulletList [ plainTextBlock "First item"
                                , plainTextBlock "Second item"
                                ]))
        it "unwraps line continuation in bullet list slides" $ do
            parseSlide "" "\n## Slide Title\n\n- First item\n- Second\n  item"
                `shouldBe` Right (SingleContentSlide
                    "Slide Title"
                    (BulletList [ plainTextBlock "First item"
                                , plainTextBlock "Second item"
                                ]))
    describe "parseSlideshow" $ do
        it "parses a title-only slideshow" $ do
            parseSlideshow' testSlideshowTitleOnly `shouldBe`
                Right expectedSlideshowTitleOnly
        it "parses a title-only slideshow with trailing newline" $ do
            parseSlideshow' (trailingNewline testSlideshowTitleOnly) `shouldBe`
                Right expectedSlideshowTitleOnly
        it "strips trailing whitespace from title" $ do
            parseSlideshow' (trailingWhitespace testSlideshowTitleOnly) `shouldBe`
                Right expectedSlideshowTitleOnly
        it "fails to parse a title without space after the number sign" $ do
            parseSlideshow' testSlideshowTitleOnlyMissingSpace
                `shouldSatisfy` isLeft
        it "parses a title and subtitle slideshow" $ do
            parseSlideshow' testSlideshowTitleSubtitle `shouldBe`
                Right expectedSlideshowTitleSubtitle
        it "parses a title and subtitle slideshow with trailing newline" $ do
            parseSlideshow' (trailingNewline testSlideshowTitleSubtitle) `shouldBe`
                Right expectedSlideshowTitleSubtitle
        it "fails to parse multiline subtitle" $ do
            parseSlideshow' testSlideshowMultilineSubtitle `shouldSatisfy`
                isLeft
        it "parses a title and minimal copyright slideshow" $ do
            parseSlideshow' testSlideshowTitleMinimalCopyright `shouldBe`
                Right expectedSlideshowTitleMinimalCopyright
        it "parses a title and single-year copyright slideshow" $ do
            parseSlideshow' testSlideshowTitleSingleYearCopyright `shouldBe`
                Right expectedSlideshowTitleSingleYearCopyright
        it "requires space between copyright year and author" $ do
            parseSlideshow' testSlideshowTitleSingleYearCopyrightNoSpace `shouldSatisfy`
                isLeft
        it "parses a title and year range copyright slideshow" $ do
            parseSlideshow' testSlideshowTitleYearRangeCopyright `shouldBe`
                Right expectedSlideshowTitleYearRangeCopyright
        it "parses a title and compact year range copyright slideshow" $ do
            parseSlideshow' testSlideshowTitleCompactYearRangeCopyright `shouldBe`
                Right expectedSlideshowTitleYearRangeCopyright
        it "parses an empty slide" $ do
            parseSlideshow' testSlideshowEmptySlide `shouldBe`
                Right expectedSlideshowEmptySlide
        it "parses an empty slide after a subtitle" $ do
            parseSlideshow' testSlideshowSubtitleEmptySlide `shouldBe`
                Right expectedSlideshowSubtitleEmptySlide
        it "parses a bullet list slide" $ do
            parseSlideshow' testSlideshowBulletListSlide `shouldBe`
                Right expectedSlideshowBulletListSlide
        it "does not accept mixing bullet characters in a list" $ do
            parseSlideshow' testSlideshowInvalidBulletListSlide `shouldSatisfy`
                isLeft
        it "parses multiple empty slides" $ do
            parseSlideshow' testSlideshowTwoEmptySlides `shouldBe`
                Right expectedSlideshowTwoEmptySlides
        it "parses multiple bullet list slides" $ do
            parseSlideshow' testSlideshowTwoBulletListSlides `shouldBe`
                Right expectedSlideshowTwoBulletListSlides
        it "parses mixed slides" $ do
            parseSlideshow' testSlideshowMixedSlides `shouldBe`
                Right expectedSlideshowMixedSlides
  where
    parseSlideshow' = parseSlideshow ""
    testSlideshowTitleOnly = "# Slideshow Title"
    expectedSlideshowTitleOnly =
        Slideshow Nothing [ TitleSlide "Slideshow Title" Nothing ]

    testSlideshowTitleOnlyMissingSpace = "#SlideshowTitle"

    testSlideshowTitleSubtitle =
        "# Slideshow Title\n\nSlideshow Subtitle"
    expectedSlideshowTitleSubtitle =
        Slideshow
            Nothing
            [ TitleSlide "Slideshow Title" (Just "Slideshow Subtitle") ]

    testSlideshowMultilineSubtitle =
        "# Slideshow Title\n\nSlideshow Subtitle\n    with multiple lines"

    testSlideshowTitleMinimalCopyright =
        "@copyright Slideshow Author\n\n# Slideshow Title"
    expectedSlideshowTitleMinimalCopyright =
        Slideshow
            (Just $ Copyright "Slideshow Author" Nothing)
            [ TitleSlide "Slideshow Title" Nothing ]

    testSlideshowTitleSingleYearCopyright =
        "@copyright 2022 Slideshow Author\n\n# Slideshow Title"
    expectedSlideshowTitleSingleYearCopyright =
        Slideshow
            (Just $ Copyright "Slideshow Author" (Just $ SingleYear 2022))
            [ TitleSlide "Slideshow Title" Nothing ]

    testSlideshowTitleSingleYearCopyrightNoSpace =
        "@copyright 2022Slideshow Author\n\n# Slideshow Title"

    testSlideshowTitleYearRangeCopyright =
        "@copyright 2020 - 2022 Slideshow Author\n\n# Slideshow Title"
    testSlideshowTitleCompactYearRangeCopyright =
        "@copyright 2020-2022 Slideshow Author\n\n# Slideshow Title"
    expectedSlideshowTitleYearRangeCopyright =
        Slideshow
            (Just $ Copyright "Slideshow Author" (Just $ YearRange 2020 2022))
            [ TitleSlide "Slideshow Title" Nothing ]

    testSlideshowEmptySlide =
        "# Slideshow Title\n\n## Slide Title"
    expectedSlideshowEmptySlide =
        Slideshow
            Nothing
            [ TitleSlide "Slideshow Title" Nothing
            , SingleContentSlide "Slide Title" NoContent
            ]

    testSlideshowSubtitleEmptySlide =
        "# Slideshow Title\n\nSlideshow Subtitle\n\n## Slide Title"
    expectedSlideshowSubtitleEmptySlide =
        Slideshow
            Nothing
            [ TitleSlide "Slideshow Title" (Just "Slideshow Subtitle")
            , SingleContentSlide "Slide Title" NoContent
            ]

    testSlideshowTwoEmptySlides =
        "# Slideshow Title\n\n## Slide Title\n\n## Second Slide Title\n"
    expectedSlideshowTwoEmptySlides =
        Slideshow
            Nothing
            [ TitleSlide "Slideshow Title" Nothing
            , SingleContentSlide "Slide Title" NoContent
            , SingleContentSlide "Second Slide Title" NoContent
            ]

    testSlideshowBulletListSlide =
        "# Slideshow Title\n\n## Slide Title\n\n- First item\n- Second item"
    expectedSlideshowBulletListSlide =
        Slideshow
            Nothing
            [ TitleSlide "Slideshow Title" Nothing
            , SingleContentSlide
                "Slide Title"
                (BulletList [ plainTextBlock "First item"
                            , plainTextBlock "Second item"
                            ])
            ]

    testSlideshowTwoBulletListSlides =
        "# Slideshow Title\n\n## Slide Title\n\n+ First item\n+ Second item\n\n## Second Slide Title\n\n* 1st item\n* 2nd item"
    expectedSlideshowTwoBulletListSlides =
        Slideshow
            Nothing
            [ TitleSlide "Slideshow Title" Nothing
            , SingleContentSlide
                  "Slide Title"
                  (BulletList [ plainTextBlock "First item"
                              , plainTextBlock "Second item"
                              ])
            , SingleContentSlide
                  "Second Slide Title"
                  (BulletList [ plainTextBlock "1st item"
                              , plainTextBlock "2nd item"
                              ])
            ]

    testSlideshowMixedSlides = T.unlines
        [ "@copyright 2020-2022 Author Authorson"
        , " "
        , "# Slideshow"
        , " "
        , "...with a subtitle"
        , " "
        , "## Empty Slide"
        , " "
        , "## List Slide"
        , " "
        , "- First item"
        , "- Second item"
        , " "
        , "## Second Empty Slide"
        ]
    expectedSlideshowMixedSlides =
        Slideshow
            (Just $ Copyright "Author Authorson" (Just $ YearRange 2020 2022))
            [ TitleSlide "Slideshow" (Just "...with a subtitle")
            , SingleContentSlide "Empty Slide" NoContent
            , SingleContentSlide
                  "List Slide"
                  (BulletList [ plainTextBlock "First item"
                              , plainTextBlock "Second item"
                              ])
            , SingleContentSlide "Second Empty Slide" NoContent
            ]

    testSlideshowInvalidBulletListSlide =
        "# Slideshow Title\n\n## Slide Title\n\n- First item\n* Second item"

    trailingNewline = flip T.append "\n"
    trailingWhitespace = flip T.append "    "


