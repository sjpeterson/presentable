{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Module
-- Copyright
-- License
--
-- Presentation parser for Presentable

module Presentable.Parse.Slideshow
    ( parseSlideshow
    , parseSlide
    ) where

import Data.Bifunctor ( first )
import Data.Functor ( void )
import Data.List.NonEmpty ( NonEmpty ( (:|) ) )
import Data.Maybe ( isJust )
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Void ( Void )
import Text.Megaparsec ( Parsec
                       , ParseErrorBundle ( ParseErrorBundle )
                       , Token
                       , Tokens
                       , (<|>)
                       , anySingleBut
                       , between
                       , bundleErrors
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
                                  )

type Parser = Parsec Void Text

type ParsingError = Text

parseSlideshow :: FilePath -> Text -> Either ParsingError Slideshow
parseSlideshow filePath = first handleError . runParser slideshowParser filePath

parseSlide :: FilePath -> Text -> Either ParsingError Slide
parseSlide filePath = first handleError . runParser slideParser filePath

handleError :: ParseErrorBundle Text Void -> ParsingError
handleError = T.pack . errorBundlePretty

slideshowParser :: Parser Slideshow
slideshowParser = do
    copyright <- optional copyrightParser
    title <- heading1Parser
    subtitle <- optional $ try solitaryLineParser
    let titleSlide = TitleSlide title subtitle
    slides <- many slideParser
    return $ Slideshow copyright (titleSlide :| slides)

heading1Parser :: Parser Text
heading1Parser = between (string "# ") (optional eol) nonTag

solitaryLineParser :: Parser Text
solitaryLineParser = between
    emptyLine
    (choice [eof, (optional eol >> lookAhead emptyLine)])
    nonTag

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

copyrightYearParser :: Parser CopyrightYear
copyrightYearParser = do
    from <- yearParser
    to <- optional ((try (string " - ") <|> string "-") >> yearParser)
    return $ case to of
        Nothing  -> SingleYear from
        Just to' -> YearRange from to'

yearParser :: Parser Int
yearParser = read <$> count 4 digitChar

slideParser :: Parser Slide
slideParser = do
    title <- heading2Parser
    content <- slideContentParser
    return $ SingleContentSlide title content

heading2Parser :: Parser Text
heading2Parser = between
    (emptyLine >> string "## ")
    (optional eol)
    nonTag

slideContentParser :: Parser SlideContent
slideContentParser = try bulletListParser <|> noContentParser

bulletListParser :: Parser SlideContent
bulletListParser = emptyLine >> BulletList <$> some bulletListItemParser

bulletListItemParser :: Parser Text
bulletListItemParser = between (string "- ") (optional eol) (continuedLine 2)

noContentParser :: Parser SlideContent
noContentParser = lookAhead (space >> try eof <|> void (char '#'))
               >> return NoContent

-- ------- --
-- Helpers --
-- ------- --

nonTag :: Parser Text
nonTag = lookAhead (noneOf ['#', '@', ' ']) >> restOfLine

emptyLine :: Parser ()
emptyLine = hspace >> choice [eof, void eol]

withLeadingSpace :: Parser a -> Parser a
withLeadingSpace p = char ' ' >> p

restOfLine :: Parser Text
restOfLine = T.strip . T.pack <$> some (anySingleBut '\n')

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

