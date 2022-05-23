{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Module
-- Copyright
-- License
--
-- Presentation parser for Presentable

module Presentable.Parse.Presentation
    ( parsePresentation
    , parseSlide
    ) where

import Data.Bifunctor ( first )
import Data.Functor ( void )
import Data.Maybe ( isJust )
import Data.Text ( Text, pack, strip )
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

import Presentable.Data.Presentation ( Copyright ( Copyright )
                                     , CopyrightYear ( SingleYear, YearRange )
                                     , Presentation ( Presentation )
                                     , Slide ( Slide )
                                     , SlideContent ( BulletList, NoContent )
                                     )

type Parser = Parsec Void Text

type ParsingError = Text

parsePresentation :: FilePath -> Text -> Either ParsingError Presentation
parsePresentation filePath = first handleError . runParser presentationParser filePath

parseSlide :: FilePath -> Text -> Either ParsingError Slide
parseSlide filePath = first handleError . runParser slideParser filePath

handleError :: ParseErrorBundle Text Void -> ParsingError
handleError = pack . errorBundlePretty

presentationParser :: Parser Presentation
presentationParser = do
    copyright <- optional copyrightParser
    title <- heading1Parser
    subtitle <- optional $ try solitaryLineParser
    slides <- many slideParser
    return $ Presentation title subtitle copyright slides

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
    return $ Slide title content

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
bulletListItemParser = between (string "- ") (optional eol) nonTag

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
restOfLine = strip . pack <$> some (anySingleBut '\n')

