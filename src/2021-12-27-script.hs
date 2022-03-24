-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Text.HTML.Scalpel ( scrapeURL, attrs, Scraper, ScraperT )
import Prelude (putStrLn, IO, String, Maybe (Just), Char, mapM_, ($))

main :: IO ()
main =  do
    images <- scrapeURL "https://github.com/"  $ attrs "src" (_ "img")
    -- case images of 
    --     Just ar -> mapM_ putStrLn ar
    --     _ -> putStrLn "Error"
    putStrLn "End"