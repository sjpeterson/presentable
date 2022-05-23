{-# LANGUAGE OverloadedStrings #-}

module Presentable.Parse.PresentationSpec where

import Data.Either ( isLeft )
import qualified Data.Text as T
import Test.Hspec ( Spec, describe, it, shouldBe, shouldSatisfy )

import Presentable.Data.Presentation ( Copyright (..)
                                     , CopyrightYear (..)
                                     , Presentation (..)
                                     , Slide (..)
                                     , SlideContent (..)
                                     )
import Presentable.Parse.Presentation ( parsePresentation, parseSlide )

spec :: Spec
spec = do
    describe "parseSlide" $ do
        it "parses an empty slide" $ do
            parseSlide "" "\n## Slide Title" `shouldBe`
                Right (Slide "Slide Title" NoContent)
    describe "parsePresentation" $ do
        it "parses a title-only presentation" $ do
            parsePresentation' testPresentationTitleOnly `shouldBe`
                Right expectedPresentationTitleOnly
        it "parses a title-only presentation with trailing newline" $ do
            parsePresentation' (trailingNewline testPresentationTitleOnly) `shouldBe`
                Right expectedPresentationTitleOnly
        it "strips trailing whitespace from title" $ do
            parsePresentation' (trailingWhitespace testPresentationTitleOnly) `shouldBe`
                Right expectedPresentationTitleOnly
        it "fails to parse a title without space after the number sign" $ do
            parsePresentation' testPresentationTitleOnlyMissingSpace
                `shouldSatisfy` isLeft
        it "parses a title and subtitle presentation" $ do
            parsePresentation' testPresentationTitleSubtitle `shouldBe`
                Right expectedPresentationTitleSubtitle
        it "parses a title and subtitle presentation with trailing newline" $ do
            parsePresentation' (trailingNewline testPresentationTitleSubtitle) `shouldBe`
                Right expectedPresentationTitleSubtitle
        it "fails to parse multiline subtitle" $ do
            parsePresentation' testPresentationMultilineSubtitle `shouldSatisfy`
                isLeft
        it "parses a title and minimal copyright presentation" $ do
            parsePresentation' testPresentationTitleMinimalCopyright `shouldBe`
                Right expectedPresentationTitleMinimalCopyright
        it "parses a title and single-year copyright presentation" $ do
            parsePresentation' testPresentationTitleSingleYearCopyright `shouldBe`
                Right expectedPresentationTitleSingleYearCopyright
        it "requires space between copyright year and author" $ do
            parsePresentation' testPresentationTitleSingleYearCopyrightNoSpace `shouldSatisfy`
                isLeft
        it "parses a title and year range copyright presentation" $ do
            parsePresentation' testPresentationTitleYearRangeCopyright `shouldBe`
                Right expectedPresentationTitleYearRangeCopyright
        it "parses a title and compact year range copyright presentation" $ do
            parsePresentation' testPresentationTitleCompactYearRangeCopyright `shouldBe`
                Right expectedPresentationTitleYearRangeCopyright
        it "parses an empty slide" $ do
            parsePresentation' testPresentationEmptySlide `shouldBe`
                Right expectedPresentationEmptySlide
        it "parses an empty slide after a subtitle" $ do
            parsePresentation' testPresentationSubtitleEmptySlide `shouldBe`
                Right expectedPresentationSubtitleEmptySlide
        it "parses a bullet list slide" $ do
            parsePresentation' testPresentationBulletListSlide `shouldBe`
                Right expectedPresentationBulletListSlide
        it "parses multiple empty slides" $ do
            parsePresentation' testPresentationTwoEmptySlides `shouldBe`
                Right expectedPresentationTwoEmptySlides
        it "parses multiple bullet list slides" $ do
            parsePresentation' testPresentationTwoBulletListSlides `shouldBe`
                Right expectedPresentationTwoBulletListSlides
        it "parses mixed slides" $ do
            parsePresentation' testPresentationMixedSlides `shouldBe`
                Right expectedPresentationMixedSlides
  where
    parsePresentation' = parsePresentation ""
    testPresentationTitleOnly = "# Presentation Title"
    expectedPresentationTitleOnly =
        Presentation "Presentation Title" Nothing Nothing []

    testPresentationTitleOnlyMissingSpace = "#PresentationTitle"

    testPresentationTitleSubtitle =
        "# Presentation Title\n\nPresentation Subtitle"
    expectedPresentationTitleSubtitle =
        Presentation "Presentation Title" (Just "Presentation Subtitle") Nothing []

    testPresentationMultilineSubtitle =
        "# Presentation Title\n\nPresentation Subtitle\n    with multiple lines"

    testPresentationTitleMinimalCopyright =
        "@copyright Presentation Author\n\n# Presentation Title"
    expectedPresentationTitleMinimalCopyright =
        Presentation
            "Presentation Title"
            Nothing
            (Just $ Copyright "Presentation Author" Nothing)
            []

    testPresentationTitleSingleYearCopyright =
        "@copyright 2022 Presentation Author\n\n# Presentation Title"
    expectedPresentationTitleSingleYearCopyright =
        Presentation
            "Presentation Title"
            Nothing
            (Just $ Copyright "Presentation Author" (Just $ SingleYear 2022))
            []

    testPresentationTitleSingleYearCopyrightNoSpace =
        "@copyright 2022Presentation Author\n\n# Presentation Title"

    testPresentationTitleYearRangeCopyright =
        "@copyright 2020 - 2022 Presentation Author\n\n# Presentation Title"
    testPresentationTitleCompactYearRangeCopyright =
        "@copyright 2020-2022 Presentation Author\n\n# Presentation Title"
    expectedPresentationTitleYearRangeCopyright =
        Presentation
            "Presentation Title"
            Nothing
            (Just $ Copyright "Presentation Author" (Just $ YearRange 2020 2022))
            []

    testPresentationEmptySlide =
        "# Presentation Title\n\n## Slide Title"
    expectedPresentationEmptySlide =
        Presentation
            "Presentation Title"
            Nothing
            Nothing
            [Slide "Slide Title" NoContent]

    testPresentationSubtitleEmptySlide =
        "# Presentation Title\n\nPresentation Subtitle\n\n## Slide Title"
    expectedPresentationSubtitleEmptySlide =
        Presentation
            "Presentation Title"
            (Just "Presentation Subtitle")
            Nothing
            [Slide "Slide Title" NoContent]

    testPresentationTwoEmptySlides =
        "# Presentation Title\n\n## Slide Title\n\n## Second Slide Title\n"
    expectedPresentationTwoEmptySlides =
        Presentation
            "Presentation Title"
            Nothing
            Nothing
            [ Slide "Slide Title" NoContent
            , Slide "Second Slide Title" NoContent
            ]

    testPresentationBulletListSlide =
        "# Presentation Title\n\n## Slide Title\n\n- First item\n- Second item"
    expectedPresentationBulletListSlide =
        Presentation
            "Presentation Title"
            Nothing
            Nothing
            [Slide "Slide Title" (BulletList ["First item", "Second item"])]

    testPresentationTwoBulletListSlides =
        "# Presentation Title\n\n## Slide Title\n\n- First item\n- Second item\n\n## Second Slide Title\n\n- 1st item\n- 2nd item"
    expectedPresentationTwoBulletListSlides =
        Presentation
            "Presentation Title"
            Nothing
            Nothing
            [ Slide "Slide Title" (BulletList ["First item", "Second item"])
            , Slide "Second Slide Title" (BulletList ["1st item", "2nd item"])
            ]

    testPresentationMixedSlides = T.unlines
        [ "@copyright 2020-2022 Author Authorson"
        , " "
        , "# Presentation"
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
    expectedPresentationMixedSlides =
        Presentation
            "Presentation"
            (Just "...with a subtitle")
            (Just $ Copyright "Author Authorson" (Just $ YearRange 2020 2022))
            [ Slide "Empty Slide" NoContent
            , Slide "List Slide" (BulletList ["First item", "Second item"])
            , Slide "Second Empty Slide" NoContent
            ]

    trailingNewline = flip T.append "\n"
    trailingWhitespace = flip T.append "    "


