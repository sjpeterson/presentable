{-# LANGUAGE OverloadedStrings #-}

module Presentable.Parse.Slideshow (
    parseSlideshow,
    parseSlide,
) where

import Control.Monad (unless)
import qualified Control.Monad.Combinators.NonEmpty as NE
import Data.Bifunctor (first)
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (
    ParseErrorBundle,
    Parsec,
    anySingleBut,
    between,
    choice,
    count,
    eof,
    errorBundlePretty,
    lookAhead,
    many,
    noneOf,
    optional,
    runParser,
    some,
    try,
    (<|>),
 )
import Text.Megaparsec.Char (char, digitChar, eol, hspace, space, string)

import Presentable.Data.Slideshow (
    BulletList (BulletList),
    BulletListItem (BulletListItem),
    Copyright (Copyright),
    CopyrightYear (SingleYear, YearRange),
    Slide (SingleContentSlide, TitleSlide),
    SlideContent (BulletListContent, NoContent, PlainTextContent),
    Slideshow (Slideshow),
 )
import Presentable.Data.TextBlock (TextBlock, plainTextBlock)

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
    title <- many emptyLine >> heading1Parser
    subtitle <- many emptyLine >> optional (try nonTag)
    _ <- optional eol
    let titleSlide = TitleSlide title subtitle
    slides <- many (many emptyLine >> slideParser)
    _ <- many emptyLine >> eof
    return $ Slideshow copyright (titleSlide :| slides)

-- | Parser for a level 1 markdown heading.
heading1Parser :: Parser Text
heading1Parser = between (string "# ") (optional eol) nonTag

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
        Nothing -> SingleYear from
        Just to' -> YearRange from to'

-- | Parser for a single year.
yearParser :: Parser Int
yearParser = read <$> count 4 digitChar

-- | Parser for a single slide.
slideParser :: Parser Slide
slideParser = do
    title <- heading2Parser
    SingleContentSlide title <$> slideContentParser

-- | Parser for a level 2 markdown heading.
heading2Parser :: Parser Text
heading2Parser =
    between
        (string "## ")
        (optional eol)
        nonTag

-- | Parser for slide contents.
slideContentParser :: Parser SlideContent
slideContentParser =
    try bulletListParser
        <|> try plainTextParser
        <|> try noContentParser

bulletMarkers :: [Char]
bulletMarkers = ['-', '*', '+']

-- | Parser for a bullet list.
bulletListParser :: Parser SlideContent
bulletListParser =
    emptyLine
        >> BulletListContent . BulletList
            <$> choice (map (NE.some . bulletListItemParser 0) bulletMarkers)

-- | Parser for an item in a bullet list.
bulletListItemParser :: Int -> Char -> Parser BulletListItem
bulletListItemParser indent bulletChar = do
    _ <- string $ T.pack $ replicate indent ' ' ++ [bulletChar, ' ']
    itemText <- plainTextBlock <$> continuedLine (indent + 2) True
    _ <- optional eol
    sublist <-
        fmap (fmap BulletList) $
            optional $
                choice $
                    map
                        (NE.some . bulletListItemParser (indent + 2))
                        bulletMarkers
    return $ BulletListItem itemText sublist

-- | Parser for plain text slide content.
plainTextParser :: Parser SlideContent
plainTextParser = PlainTextContent <$> NE.some textBlockParser

-- | Parser for a single text block.
textBlockParser :: Parser TextBlock
textBlockParser =
    plainTextBlock
        <$> between emptyLine (optional eol) (continuedLine 0 True)

-- | Parser for the content of an empty slide.
noContentParser :: Parser SlideContent
noContentParser =
    many emptyLine
        >> lookAhead (space >> try eof <|> void (char '#'))
        >> return NoContent

-- ------- --

-- | Parser for any text until EOL that is not a tag.
nonTag :: Parser Text
-- Helpers --
-- ------- --
nonTag = lookAhead (noneOf ['#', '@', ' ']) >> restOfLine

-- | Parser for an empty line.
emptyLine :: Parser ()
emptyLine = hspace >> void eol -- choice [eof, void eol]

-- | Parser for Text until EOL.
restOfLine :: Parser Text
restOfLine = T.strip . T.pack <$> some (anySingleBut '\n')

-- | Creates a parser for lines with continuation.
continuedLine :: Int -> Bool -> Parser Text
continuedLine indentationLevel firstLine = do
    void $ lookAhead $ anySingleBut '#'
    unless firstLine (void $ lookAhead $ noneOf ['-', '*', '+'])
    first <- T.strip . T.pack <$> some (anySingleBut '\n')
    continuation <-
        optional $
            try (continuationMatch >> continuedLine indentationLevel False)
    return $ case continuation of
        Nothing -> first
        Just c -> T.unwords [first, c]
  where
    continuationMatch =
        string (T.pack $ '\n' : replicate indentationLevel ' ')
            >> lookAhead (noneOf ['-', '*', '+'])
