module BlogLenses where

import Lens
import Blog
import Time

entryWOKey :: Lens (Entry,[Tag]) (String,String,String,CalendarTime,[Tag])
entryWOKey = isoLens inn out <.> keepFst
 where
  inn (k, (t1',t2',a',d',tags')) = (Entry k t1' t2' a' d', tags')
  out (Entry k t1 t2 a d, tags)  = (k, (t1,t2,a,d,tags))

-- projection from `Entry` to title, text and tags
entryZoom :: Lens (Entry,[Tag]) (String, String, [Tag])
entryZoom = isoLens inn out <.> keepFst
 where
  inn ((k, a, d), (t1', t2', tags')) = (Entry k t1' t2' a d, tags')
  out (Entry k t1' t2' a d, tags') = ((k, a, d), (t1',t2',tags'))

-- projection from `Comment` to text and entry
commentWOKey :: Entry -> Lens Comment (String, String, CalendarTime, Entry)
commentWOKey entry = isoLens inn (out entry) <.> keepFst
 where
  inn ((cKey,eKey), (t',a',d',e)) =
    setCommentEntryCommentingKey (Comment cKey t' a' d' eKey)
                                 (entryKey e)
  out entry (Comment cKey t' a' d' eKey) = ((cKey,eKey),(t',a',d',entry))

-- projection from `Comment` to text and entry
commentZoom :: Entry -> Lens Comment (String,Entry)
commentZoom entry = isoLens inn (out entry) <.> keepFst
 where
  inn ((cKey,a',d',eKey), (t',e)) =
    setCommentEntryCommentingKey (Comment cKey t' a' d' eKey)
                                 (entryKey e)
  out entry (Comment cKey t' a' d' eKey) = ((cKey,a',d',eKey),(t',entry))

tagWOKey :: Lens Tag String
tagWOKey = isoLens inn out <.> keepFst
 where
  inn (key,name)     = Tag key name
  out (Tag key name) = (key,name)

-- projection from `CalendarTime` to day, month, year
dateLens :: Lens CalendarTime (Int,Int,Int)
dateLens = isoLens inn out
 where
  inn (day, month, year) = CalendarTime year month day 0 0 0 0
  out (CalendarTime year month day _ _ _ _) = (day, month, year)

negationLens :: Lens Bool Bool
negationLens = isoLens not not