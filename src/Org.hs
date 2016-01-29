{-# LANGUAGE MultiWayIf #-}
module Org where
import           Data.List
import qualified Data.Map.Lazy         as M
import           Data.Monoid
import qualified Data.Set              as S
import qualified Data.Text.Lazy        as T
import           Data.Time
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Debug.Trace
import           Safe
import           Text.ICalendar


-- properties not kept in ical export
data OrgEntry = OrgEntry
    { entryTitle    :: Maybe String
    , entrySched    :: Maybe EntryDate
    , entryDead     :: Maybe EntryDate
    , entryContents :: Maybe String
    , entryTags     :: [String]
    , entryFile     :: Maybe String
    }

{-
** TODO [#priority] title            :@home:NonSchool:
   SCHEDULED: <2016-02-01 Mon 19:00 ++1w>
or DEADLINE: <2016-02-01 Mon 19:00 ++1w>
   <contents>
   -}


instance Show OrgEntry where
    show (OrgEntry title sched dead cont tags file) =
        let firstLine =
                "** TODO " ++
                fromJustDef "" title ++ "    " ++ showTags tags ++ "\n"
            dateLine =
                case sched of
                    Just date -> "\tSCHEDULED: " ++ showDate date ++ "\n"
                    Nothing ->
                        case dead of
                            Just date ->
                                "\tDEADLINE: " ++ showDate date ++ "\n"
                            Nothing -> ""
            content =
                case cont of
                    Nothing -> ""
                    Just c -> unlines $ ("\t" ++) <$> (lines $ tr c)
        in mconcat [firstLine, dateLine, content, "\n"]
      where
        tr =
            map
                (\c ->
                      if c == '•'
                          then '-'
                          else c)
        showTags [] = ""
        showTags ts = ":" ++ intercalate ":" ts ++ ":"
        showDate date =
            let dateTime = edDate date
            in concat
                   [ "<"
                   , case dateTime of
                         DTStartDateTime dt _ ->
                             case dt of
                                 FloatingDateTime f ->
                                     formatTime
                                         defaultTimeLocale
                                         "%Y-%m-%d %a %R"
                                         f
                                 UTCDateTime f ->
                                     formatTime
                                         defaultTimeLocale
                                         "%Y-%m-%d %a %R"
                                         f
                                 ZonedDateTime f _ ->
                                     formatTime
                                         defaultTimeLocale
                                         "%Y-%m-%d %a %R"
                                         f
                         DTStartDate d _ ->
                             formatTime defaultTimeLocale "%Y-%m-%d %a" $
                             dateValue d
                   , ">"]




-- ignoring end datetimes for now
-- TODO: recur not used yet
data EntryDate = EntryDate
    { edDate  :: DTStart
    , edRecur :: Maybe Recur
    } deriving (Show)

-- ignoring lots of stuff for now
-- events :: [VEvent]
orgify :: VCalendar -> String
orgify cal =
    let eventMap = vcEvents cal
        events = snd <$> M.toList eventMap
        entries = toEntry <$> events
    in ("* FromICal\n" ++) $ unlines $ show <$> entries


toEntry :: VEvent -> OrgEntry
toEntry event =
    let dateTime = veDTStart event
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
        (sched,dead,title') =
            case dateTime of
                Nothing -> (Nothing, Nothing, title)
                Just dt ->
                    case title of
                        Nothing ->
                            (Just $ EntryDate dt recur, Nothing, title) -- default to sched
                        Just t ->
                            if |  "S: " `isPrefixOf` t ->
                                   ( Just $ EntryDate dt recur
                                   , Nothing
                                   , drop 3 <$> title)
                               |  "DL: " `isPrefixOf` t ->
                                   ( Nothing
                                   , Just $ EntryDate dt recur
                                   , drop 4 <$> title)
                               |  otherwise ->
                                   ( Just $ EntryDate dt recur
                                   , Nothing
                                   , title) -- default to sched
    in OrgEntry
       { entryTitle = title'
       , entrySched = sched
       , entryDead = dead
       , entryContents = contents
       , entryTags = tags
       , entryFile = file
       }
