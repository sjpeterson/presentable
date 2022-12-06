module Presentable.Parse.Commonmark where

-- commonmark :: IsBlock il bl => String -> Text -> Either ParseError bl
--
-- cf. parseSlideshow :: FilePath -> Text -> Either ParsingError Slideshow
--
-- ... so basically need to make Slideshow an instance of IsBlock and Bob's yer
--     uncle. See (https://hackage.haskell.org/package/commonmark-0.2.2/docs/src/Commonmark.Html.html)
--     for inspiration.
