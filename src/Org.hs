{-# LANGUAGE MultiWayIf #-}
module Org where
import           Data.List
import qualified Data.Map.Lazy         as M
import           Data.Monoid
import qualified Data.Set              as S
import qualified Data.Text.Lazy        as T
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Debug.Trace
import           Text.ICalendar

-- properties not kept in ical export
data OrgEntry = OrgEntry
    { entryTitle    :: Maybe String
    , entrySched    :: Maybe EntryDate
    , entryDead     :: Maybe EntryDate
    , entryContents :: Maybe String
    , entryTags     :: [String]
    , entryFile     :: Maybe String
    } deriving (Show)

-- instance Show OrgEntry where
--     show (OrgEntry title sched dead cont tags file) = undefined

data EntryDate = EntryDate
    { edDate  :: UTCTime
    , edRecur :: Maybe Recur
    } deriving (Show)

-- ignoring lots of stuff for now
-- events :: [VEvent]
orgify :: VCalendar -> String
orgify cal =
    let eventMap = vcEvents cal
        events = snd <$> M.toList eventMap
        entries = toEntry <$> events
    in show entries


toEntry :: VEvent -> OrgEntry
toEntry event =
    let dateTime = dtStampValue $ veDTStamp event
        contents = T.unpack <$> descriptionValue <$> veDescription event
        title = T.unpack <$> summaryValue <$> veSummary event
        tagsList =
            concatMap (S.toList . categoriesValues) $
            S.toList $ veCategories event
        tags =
            case tagsList of
                [] -> []
                [f] -> []
                _ -> T.unpack <$> init tagsList
        file =
            case tagsList of
                [] -> Nothing
                _ -> Just $ T.unpack $ last tagsList
        recur =
            case S.toList (veRRule event) of
                [] -> Nothing
                l -> Just $ rRuleValue $ head l
        -- what if something is scheduled and has deadline?
        (sched,dead) =
            case title of
                Nothing -> (Just $ EntryDate dateTime recur, Nothing) -- default to sched
                Just t ->
                    if | "S: " `isPrefixOf` t ->  (Just $ EntryDate dateTime recur, Nothing)
                       | "DL: " `isPrefixOf` t -> (Nothing, Just $ EntryDate dateTime recur)
                       | otherwise -> (Just $ EntryDate dateTime recur, Nothing) -- default to sched
    in OrgEntry
       { entryTitle = title
       , entrySched = sched
       , entryDead = dead
       , entryContents = contents
       , entryTags = tags
       , entryFile = file
       }
