module Main where
import           Data.Default
import           System.Environment
import           Text.ICalendar

import           Org                (orgify)


main :: IO ()
main = do
    [file] <- getArgs
    res <- parseICalendarFile def file
    case res of
        Left err -> putStrLn err
        Right ([cal],msgs) -> do
            let orgText = orgify cal
            putStrLn orgText
