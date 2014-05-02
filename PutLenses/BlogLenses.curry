module BlogLenses where

import Lens
import Blog
import Time

entryWOKey :: Lens (Entry,[Tag]) (String,String,String,CalendarTime,[Tag])
entryWOKey (Entry k _ _ _ _, _) (t1',t2',a',d',tags') =
    (Entry k t1' t2' a' d', tags')

-- projection from `Entry` to title, text and tags
entryZoom :: Lens (Entry,[Tag]) (String, String, [Tag])
entryZoom  (Entry k _ _ a d, _) (t1',t2',tags') =
  (Entry k t1' t2' a d, tags')

commentWOKey :: Entry -> Lens Comment (String,String,CalendarTime,Entry)
commentWOKey _ (Comment cKey _ _ _ eKey) (text',author',date',entry) =
  setCommentEntryCommentingKey (Comment cKey text' author' date' eKey)
                                 (entryKey entry)
-- projection from `Comment` to text and entry
commentZoom :: Entry -> Lens Comment (String,Entry)
commentZoom _ (Comment cKey _ author date eKey) (text',entry) =
  setCommentEntryCommentingKey (Comment cKey text' author date eKey)
                                 (entryKey entry)

tagWOKey :: Lens Tag String
tagWOKey (Tag key _) name' = Tag key name'

-- projection from `CalendarTime` to day, month, year
dateLens :: Lens CalendarTime (Int,Int,Int)
dateLens (CalendarTime _ _ _ h m s tz) (day, month, year) = CalendarTime year month day h m s tz

-- negation lens
negationLens :: Lens Bool Bool
negationLens b _ = not b
