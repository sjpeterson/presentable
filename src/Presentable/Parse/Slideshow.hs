{-# LANGUAGE OverloadedStrings #-}

module Presentable.Parse.Slideshow
    ( parseSlideshow
    , parseSlide
    ) where

import qualified Control.Monad.Combinators.NonEmpty as NE
import Data.Bifunctor ( first )
import Data.Functor ( void )
import Data.List.NonEmpty ( NonEmpty ( (:|) ) )
import Data.Maybe ( isJust )
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Void ( Void )
import Text.Megaparsec ( Parsec
                       , ParseErrorBundle
                       , (<|>)
                       , anySingleBut
                       , between
                       , choice
                       , count
                       , eof
                       , errorBundlePretty
                       , lookAhead
                       , many
                       , noneOf
                       , optional
                       , runParser
                       , some
                       , try
                       )
import Text.Megaparsec.Char ( char, digitChar, eol, hspace, space, string )

import Presentable.Data.Slideshow ( Copyright ( Copyright )
                                  , CopyrightYear ( SingleYear, YearRange )
                                  , Slideshow ( Slideshow )
                                  , Slide ( SingleContentSlide, TitleSlide )
                                  , SlideContent ( BulletList, NoContent )
                                  , TextBlock
                                  , plainTextBlock
                                  )

type Parser = Parsec Void Text

type ParsingError = Text

-- | Parse text as a slideshow.
parseSlideshow :: FilePath -> Text -> Either ParsingError Slideshow
parseSlideshow filePath = first handleError . runParser slideshowParser filePath

-- | Parse text as a single slide.
parseSlide :: FilePath -> Text -> Either ParsingError Slide
parseSlide filePath = first handleError . runParser slideParser filePath

-- | Convert Megaparsec error to own ParsingError.
handleError :: ParseErrorBundle Text Void -> ParsingError
handleError = T.pack . errorBundlePretty

-- | Parser for a complete slideshow.
slideshowParser :: Parser Slideshow
slideshowParser = do
    copyright <- optional copyrightParser
    title <- heading1Parser
    subtitle <- optional $ try solitaryLineParser
    let titleSlide = TitleSlide title subtitle
    slides <- many slideParser
    return $ Slideshow copyright (titleSlide :| slides)

-- | Parser for a level 1 markdown heading.
heading1Parser :: Parser Text
heading1Parser = between (string "# ") (optional eol) nonTag

-- | Parser for a line of text surrounded by whitespace.
solitaryLineParser :: Parser Text
solitaryLineParser = between
    emptyLine
    (choice [eof, (optional eol >> lookAhead emptyLine)])
    nonTag

-- | Parser for a copyright tag.
copyrightParser :: Parser Copyright
copyrightParser = do
    _ <- char '@'
    _ <- string "copyright"
    _ <- char ' '
    copyrightYear <- optional copyrightYearParser
    _ <- if isJust copyrightYear then char ' ' else pure ' '
    author <- restOfLine
    _ <- eol >> emptyLine
    return $ Copyright author copyrightYear

-- | Parser for the year or year range of a copyright tag.
copyrightYearParser :: Parser CopyrightYear
copyrightYearParser = do
    from <- yearParser
    to <- optional ((try (string " - ") <|> string "-") >> yearParser)
    return $ case to of
        Nothing  -> SingleYear from
        Just to' -> YearRange from to'

-- | Parser for a single year.
yearParser :: Parser Int
yearParser = read <$> count 4 digitChar

-- | Parser for a single slide.
slideParser :: Parser Slide
slideParser = do
    title <- heading2Parser
    content <- slideContentParser
    return $ SingleContentSlide title content

-- | Parser for a level 2 markdown heading.
heading2Parser :: Parser Text
heading2Parser = between
    (emptyLine >> string "## ")
    (optional eol)
    nonTag

-- | Parser for slide contents.
slideContentParser :: Parser SlideContent
slideContentParser = try bulletListParser <|> noContentParser

-- | Parser for a bullet list.
bulletListParser :: Parser SlideContent
bulletListParser = emptyLine >> BulletList <$> NE.some bulletListItemParser

-- | Parser for an item in a bullet list.
bulletListItemParser :: Parser TextBlock
bulletListItemParser =
    plainTextBlock <$> between (string "- ") (optional eol) (continuedLine 2)

-- | Parser for the content of an empty slide.
noContentParser :: Parser SlideContent
noContentParser = lookAhead (space >> try eof <|> void (char '#'))
               >> return NoContent

-- ------- --
-- Helpers --
-- ------- --

-- | Parser for any text until EOL that is not a tag.
nonTag :: Parser Text
nonTag = lookAhead (noneOf ['#', '@', ' ']) >> restOfLine

-- | Parser for an empty line.
emptyLine :: Parser ()
emptyLine = hspace >> choice [eof, void eol]

-- | Parser for Text until EOL.
restOfLine :: Parser Text
restOfLine = T.strip . T.pack <$> some (anySingleBut '\n')

-- | Creates a parser for lines with continuation.
continuedLine :: Int -> Parser Text
continuedLine indentationLevel = do
    first <- T.strip . T.pack <$> some (anySingleBut '\n')
    continuation <- optional $
        try (continuationMatch >> continuedLine indentationLevel)
    return $ case continuation of
        Nothing -> first
        Just c -> T.unwords [first, c]
  where
    continuationMatch =
        string $ T.append "\n" (T.replicate indentationLevel " ")
