module Blog (
 Entry(..), Comment(..), Tag(..), EntryKey, CommentKey, TagKey, Tagging, entryTitle,
 setEntryTitle, entryText, setEntryText, entryAuthor, setEntryAuthor,
 entryDate, setEntryDate, commentText, setCommentText, commentAuthor,
 setCommentAuthor, commentDate, setCommentDate, commentEntryCommentingKey,
 setCommentEntryCommentingKey, tagName, setTagName, entry, entryKey,
 showEntryKey, readEntryKey, newEntry, updateEntry, deleteEntry, getEntry,
 queryAllEntrys, queryCondEntry, comment, commentKey, showCommentKey,
 readCommentKey, newCommentWithEntryCommentingKey, updateComment,
 deleteComment, getComment, queryAllComments, queryCondComment, tag, tagKey,
 showTagKey, readTagKey, newTag, updateTag, deleteTag, getTag, queryAllTags,
 queryCondTag, taggingEntryTaggingKey, taggingTagTaggingKey, tagging,
 newTagging, deleteTagging, getEntryTags, queryAllTaggings, queryCondTagging,
 tagged, tags, commenting, isCommentedBy, commentsOn, checkAllData,
 checkTagging, checkEntry, checkComment, checkTag, saveAllData, restoreAllData
 ) where

import ERDGeneric
import KeyDatabase
import Time

data Entry = Entry ERDGeneric.Key String String String Time.CalendarTime 
type EntryTuple = (String,String,String,Time.CalendarTime)
data Comment
 = Comment ERDGeneric.Key String String Time.CalendarTime ERDGeneric.Key 
type CommentTuple = (String,String,Time.CalendarTime,ERDGeneric.Key)
data Tag = Tag ERDGeneric.Key String 
type TagTuple = String
data EntryKey = EntryKey ERDGeneric.Key 
data CommentKey = CommentKey ERDGeneric.Key 
data TagKey = TagKey ERDGeneric.Key 
data Tagging = Tagging ERDGeneric.Key ERDGeneric.Key 
type TaggingTuple = (ERDGeneric.Key,ERDGeneric.Key)

--- Transforms entity Entry into tuple representation.
entry2tuple :: Entry -> EntryTuple
entry2tuple (Entry _ x2 x3 x4 x5) = (x2,x3,x4,x5)

--- Transforms key and tuple into a Entry entity.
keytuple2Entry :: ERDGeneric.Key -> EntryTuple -> Entry
keytuple2Entry x1 (x2 ,x3 ,x4 ,x5) = Entry x1 x2 x3 x4 x5

--- Transforms entity Comment into tuple representation.
comment2tuple :: Comment -> CommentTuple
comment2tuple (Comment _ x2 x3 x4 x5) = (x2,x3,x4,x5)

--- Transforms key and tuple into a Comment entity.
keytuple2Comment :: ERDGeneric.Key -> CommentTuple -> Comment
keytuple2Comment x1 (x2 ,x3 ,x4 ,x5) = Comment x1 x2 x3 x4 x5

--- Transforms entity Tag into tuple representation.
tag2tuple :: Tag -> TagTuple
tag2tuple (Tag _ x2) = x2

--- Transforms key and tuple into a Tag entity.
keytuple2Tag :: ERDGeneric.Key -> TagTuple -> Tag
keytuple2Tag x1 x2 = Tag x1 x2

--- Transforms relationship entity Tagging into tuple representation.
tagging2tuple :: Tagging -> TaggingTuple
tagging2tuple (Tagging x1 x2) = (x1,x2)

--- Transforms key and tuple into a Tagging relationship entity.
keytuple2Tagging :: ERDGeneric.Key -> TaggingTuple -> Tagging
keytuple2Tagging _ (x1 ,x2) = Tagging x1 x2

--- Sets the value of attribute "EntryTaggingKey" in a Tagging entity.
setTaggingEntryTaggingKey :: Tagging -> EntryKey -> Tagging
setTaggingEntryTaggingKey (Tagging _ x2) x = Tagging (entryKeyToKey x) x2

--- Sets the value of attribute "TagTaggingKey" in a Tagging entity.
setTaggingTagTaggingKey :: Tagging -> TagKey -> Tagging
setTaggingTagTaggingKey (Tagging x1 _) x = Tagging x1 (tagKeyToKey x)

--- Sets the value of attribute "Key" in a Entry entity.
setEntryKey :: Entry -> ERDGeneric.Key -> Entry
setEntryKey (Entry _ x2 x3 x4 x5) x = Entry x x2 x3 x4 x5

--- Gets the value of attribute "Title" of a Entry entity.
entryTitle :: Entry -> String
entryTitle (Entry _ x _ _ _) = x

--- Sets the value of attribute "Title" in a Entry entity.
setEntryTitle :: Entry -> String -> Entry
setEntryTitle (Entry x1 _ x3 x4 x5) x = Entry x1 x x3 x4 x5

--- Gets the value of attribute "Text" of a Entry entity.
entryText :: Entry -> String
entryText (Entry _ _ x _ _) = x

--- Sets the value of attribute "Text" in a Entry entity.
setEntryText :: Entry -> String -> Entry
setEntryText (Entry x1 x2 _ x4 x5) x = Entry x1 x2 x x4 x5

--- Gets the value of attribute "Author" of a Entry entity.
entryAuthor :: Entry -> String
entryAuthor (Entry _ _ _ x _) = x

--- Sets the value of attribute "Author" in a Entry entity.
setEntryAuthor :: Entry -> String -> Entry
setEntryAuthor (Entry x1 x2 x3 _ x5) x = Entry x1 x2 x3 x x5

--- Gets the value of attribute "Date" of a Entry entity.
entryDate :: Entry -> Time.CalendarTime
entryDate (Entry _ _ _ _ x) = x

--- Sets the value of attribute "Date" in a Entry entity.
setEntryDate :: Entry -> Time.CalendarTime -> Entry
setEntryDate (Entry x1 x2 x3 x4 _) x = Entry x1 x2 x3 x4 x

--- Sets the value of attribute "Key" in a Comment entity.
setCommentKey :: Comment -> ERDGeneric.Key -> Comment
setCommentKey (Comment _ x2 x3 x4 x5) x = Comment x x2 x3 x4 x5

--- Gets the value of attribute "Text" of a Comment entity.
commentText :: Comment -> String
commentText (Comment _ x _ _ _) = x

--- Sets the value of attribute "Text" in a Comment entity.
setCommentText :: Comment -> String -> Comment
setCommentText (Comment x1 _ x3 x4 x5) x = Comment x1 x x3 x4 x5

--- Gets the value of attribute "Author" of a Comment entity.
commentAuthor :: Comment -> String
commentAuthor (Comment _ _ x _ _) = x

--- Sets the value of attribute "Author" in a Comment entity.
setCommentAuthor :: Comment -> String -> Comment
setCommentAuthor (Comment x1 x2 _ x4 x5) x = Comment x1 x2 x x4 x5

--- Gets the value of attribute "Date" of a Comment entity.
commentDate :: Comment -> Time.CalendarTime
commentDate (Comment _ _ _ x _) = x

--- Sets the value of attribute "Date" in a Comment entity.
setCommentDate :: Comment -> Time.CalendarTime -> Comment
setCommentDate (Comment x1 x2 x3 _ x5) x = Comment x1 x2 x3 x x5

--- Gets the value of attribute "EntryCommentingKey" of a Comment entity.
commentEntryCommentingKey :: Comment -> EntryKey
commentEntryCommentingKey (Comment _ _ _ _ x) = EntryKey x

--- Sets the value of attribute "EntryCommentingKey" in a Comment entity.
setCommentEntryCommentingKey :: Comment -> EntryKey -> Comment
setCommentEntryCommentingKey (Comment x1 x2 x3 x4 _) x =
  Comment x1 x2 x3 x4 (entryKeyToKey x)

--- Sets the value of attribute "Key" in a Tag entity.
setTagKey :: Tag -> ERDGeneric.Key -> Tag
setTagKey (Tag _ x2) x = Tag x x2

--- Gets the value of attribute "Name" of a Tag entity.
tagName :: Tag -> String
tagName (Tag _ x) = x

--- Sets the value of attribute "Name" in a Tag entity.
setTagName :: Tag -> String -> Tag
setTagName (Tag x1 _) x = Tag x1 x

--- Database predicate representing the relation between keys and Entry tuple entities.
entryEntry :: ERDGeneric.Key -> EntryTuple -> KeyDatabase.Dynamic
entryEntry =
  KeyDatabase.persistentSQLite "./Blog.db" "Entry"
   ["Title","Text","Author","Date"]

--- Dynamic predicate representing the relation
--- between keys and Entry entities.
entry :: EntryKey -> Entry -> KeyDatabase.Dynamic
entry key obj
  | key =:= entryKey obj = entryEntry (entryKeyToKey key) (entry2tuple obj)

--- Gets the key of a Entry entity.
entryKey :: Entry -> EntryKey
entryKey (Entry x _ _ _ _) = EntryKey x

--- Shows the key of a Entry entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showEntryKey :: Entry -> String
showEntryKey obj =
  ERDGeneric.showDatabaseKey "Entry" entryKeyToKey (entryKey obj)

--- Transforms a string into a key of a Entry entity.
--- Nothing is returned if the string does not represent a reasonable key.
readEntryKey :: String -> Maybe EntryKey
readEntryKey s = ERDGeneric.readDatabaseKey "Entry" EntryKey s

entryKeyToKey :: EntryKey -> ERDGeneric.Key
entryKeyToKey (EntryKey k) = k

maybeEntryKeyToKey :: Maybe EntryKey -> Maybe ERDGeneric.Key
maybeEntryKeyToKey Nothing = Nothing
maybeEntryKeyToKey (Just (EntryKey k)) = Just k

--- Inserts a new Entry entity.
newEntry
 :: String -> String -> String -> Time.CalendarTime
  -> KeyDatabase.Transaction Entry
newEntry title_p text_p author_p date_p =
  ERDGeneric.unique "Blog" entryEntry keytuple2Entry entryTitle title_p |>>
   ERDGeneric.newEntry entryEntry keytuple2Entry
    (title_p,text_p,author_p,date_p)

--- Updates an existing Entry entity.
updateEntry :: Entry -> KeyDatabase.Transaction ()
updateEntry entry_p =
  ERDGeneric.uniqueUpdate "Blog" entryEntry keytuple2Entry
   (entryKeyToKey . entryKey) entryTitle entry_p |>>
   KeyDatabase.updateDBEntry entryEntry (entryKeyToKey (entryKey entry_p))
    (entry2tuple entry_p)

--- Deletes an existing Entry entity.
deleteEntry :: Entry -> KeyDatabase.Transaction ()
deleteEntry entry_p =
  ERDGeneric.requiredForeignDBKey "Tagging" taggingEntry keytuple2Tagging
   taggingEntryTaggingKey (entryKey entry_p) |>>
   (ERDGeneric.requiredForeignDBKey "Comment" commentEntry keytuple2Comment
     commentEntryCommentingKey (entryKey entry_p) |>>
    KeyDatabase.deleteDBEntry entryEntry (entryKeyToKey (entryKey entry_p)))

--- Gets a Entry entity stored in the database with the given key.
getEntry :: EntryKey -> KeyDatabase.Transaction Entry
getEntry key =
  ERDGeneric.getEntry entryEntry keytuple2Entry (entryKeyToKey key)

--- Gets all Entry entities stored in the database.
queryAllEntrys :: KeyDatabase.Query [Entry]
queryAllEntrys =
  KeyDatabase.transformQ (map (uncurry keytuple2Entry))
   (KeyDatabase.allDBKeyInfos entryEntry)

--- Gets all Entry entities satisfying a given condition.
queryCondEntry :: (Entry -> Bool) -> KeyDatabase.Query [Entry]
queryCondEntry econd = KeyDatabase.transformQ (filter econd) queryAllEntrys

--- Database predicate representing the relation between keys and Comment tuple entities.
commentEntry :: ERDGeneric.Key -> CommentTuple -> KeyDatabase.Dynamic
commentEntry =
  KeyDatabase.persistentSQLite "./Blog.db" "Comment"
   ["Text","Author","Date","EntryCommentingKey"]

--- Dynamic predicate representing the relation
--- between keys and Comment entities.
comment :: CommentKey -> Comment -> KeyDatabase.Dynamic
comment key obj
  | key =:= commentKey obj = commentEntry (commentKeyToKey key)
                              (comment2tuple obj)

--- Gets the key of a Comment entity.
commentKey :: Comment -> CommentKey
commentKey (Comment x _ _ _ _) = CommentKey x

--- Shows the key of a Comment entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showCommentKey :: Comment -> String
showCommentKey obj =
  ERDGeneric.showDatabaseKey "Comment" commentKeyToKey (commentKey obj)

--- Transforms a string into a key of a Comment entity.
--- Nothing is returned if the string does not represent a reasonable key.
readCommentKey :: String -> Maybe CommentKey
readCommentKey s = ERDGeneric.readDatabaseKey "Comment" CommentKey s

commentKeyToKey :: CommentKey -> ERDGeneric.Key
commentKeyToKey (CommentKey k) = k

maybeCommentKeyToKey :: Maybe CommentKey -> Maybe ERDGeneric.Key
maybeCommentKeyToKey Nothing = Nothing
maybeCommentKeyToKey (Just (CommentKey k)) = Just k

--- Inserts a new Comment entity.
newCommentWithEntryCommentingKey
 :: String -> String -> Time.CalendarTime -> EntryKey
  -> KeyDatabase.Transaction Comment
newCommentWithEntryCommentingKey text_p author_p date_p entryCommentingKey_p =
  ERDGeneric.existsEntryWithDBKey "Entry" entryEntry
   (entryKeyToKey entryCommentingKey_p) |>>
   ERDGeneric.newEntry commentEntry keytuple2Comment
    (text_p,author_p,date_p,entryKeyToKey entryCommentingKey_p)

--- Updates an existing Comment entity.
updateComment :: Comment -> KeyDatabase.Transaction ()
updateComment comment_p =
  ERDGeneric.existsEntryWithDBKey "Entry" entryEntry
   (entryKeyToKey (commentEntryCommentingKey comment_p)) |>>
   KeyDatabase.updateDBEntry commentEntry
    (commentKeyToKey (commentKey comment_p)) (comment2tuple comment_p)

--- Deletes an existing Comment entity.
deleteComment :: Comment -> KeyDatabase.Transaction ()
deleteComment comment_p =
  KeyDatabase.deleteDBEntry commentEntry
   (commentKeyToKey (commentKey comment_p))

--- Gets a Comment entity stored in the database with the given key.
getComment :: CommentKey -> KeyDatabase.Transaction Comment
getComment key =
  ERDGeneric.getEntry commentEntry keytuple2Comment (commentKeyToKey key)

--- Gets all Comment entities stored in the database.
queryAllComments :: KeyDatabase.Query [Comment]
queryAllComments =
  KeyDatabase.transformQ (map (uncurry keytuple2Comment))
   (KeyDatabase.allDBKeyInfos commentEntry)

--- Gets all Comment entities satisfying a given condition.
queryCondComment :: (Comment -> Bool) -> KeyDatabase.Query [Comment]
queryCondComment econd =
  KeyDatabase.transformQ (filter econd) queryAllComments

--- Database predicate representing the relation between keys and Tag tuple entities.
tagEntry :: ERDGeneric.Key -> TagTuple -> KeyDatabase.Dynamic
tagEntry = KeyDatabase.persistentSQLite "./Blog.db" "Tag" ["Name"]

--- Dynamic predicate representing the relation
--- between keys and Tag entities.
tag :: TagKey -> Tag -> KeyDatabase.Dynamic
tag key obj
  | key =:= tagKey obj = tagEntry (tagKeyToKey key) (tag2tuple obj)

--- Gets the key of a Tag entity.
tagKey :: Tag -> TagKey
tagKey (Tag x _) = TagKey x

--- Shows the key of a Tag entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showTagKey :: Tag -> String
showTagKey obj = ERDGeneric.showDatabaseKey "Tag" tagKeyToKey (tagKey obj)

--- Transforms a string into a key of a Tag entity.
--- Nothing is returned if the string does not represent a reasonable key.
readTagKey :: String -> Maybe TagKey
readTagKey s = ERDGeneric.readDatabaseKey "Tag" TagKey s

tagKeyToKey :: TagKey -> ERDGeneric.Key
tagKeyToKey (TagKey k) = k

maybeTagKeyToKey :: Maybe TagKey -> Maybe ERDGeneric.Key
maybeTagKeyToKey Nothing = Nothing
maybeTagKeyToKey (Just (TagKey k)) = Just k

--- Inserts a new Tag entity.
newTag :: String -> KeyDatabase.Transaction Tag
newTag name_p =
  ERDGeneric.unique "Blog" tagEntry keytuple2Tag tagName name_p |>>
   ERDGeneric.newEntry tagEntry keytuple2Tag name_p

--- Updates an existing Tag entity.
updateTag :: Tag -> KeyDatabase.Transaction ()
updateTag tag_p =
  ERDGeneric.uniqueUpdate "Blog" tagEntry keytuple2Tag (tagKeyToKey . tagKey)
   tagName tag_p |>>
   KeyDatabase.updateDBEntry tagEntry (tagKeyToKey (tagKey tag_p))
    (tag2tuple tag_p)

--- Deletes an existing Tag entity.
deleteTag :: Tag -> KeyDatabase.Transaction ()
deleteTag tag_p =
  ERDGeneric.requiredForeignDBKey "Tagging" taggingEntry keytuple2Tagging
   taggingTagTaggingKey (tagKey tag_p) |>>
   KeyDatabase.deleteDBEntry tagEntry (tagKeyToKey (tagKey tag_p))

--- Gets a Tag entity stored in the database with the given key.
getTag :: TagKey -> KeyDatabase.Transaction Tag
getTag key = ERDGeneric.getEntry tagEntry keytuple2Tag (tagKeyToKey key)

--- Gets all Tag entities stored in the database.
queryAllTags :: KeyDatabase.Query [Tag]
queryAllTags =
  KeyDatabase.transformQ (map (uncurry keytuple2Tag))
   (KeyDatabase.allDBKeyInfos tagEntry)

--- Gets all Tag entities satisfying a given condition.
queryCondTag :: (Tag -> Bool) -> KeyDatabase.Query [Tag]
queryCondTag econd = KeyDatabase.transformQ (filter econd) queryAllTags

--- Database predicate representing the relation between keys and Tagging tuple entities.
taggingEntry :: ERDGeneric.Key -> TaggingTuple -> KeyDatabase.Dynamic
taggingEntry =
  KeyDatabase.persistentSQLite "./Blog.db" "Tagging"
   ["EntryTaggingKey","TagTaggingKey"]

taggingEntryTaggingKey :: Tagging -> EntryKey
taggingEntryTaggingKey (Tagging x _) = EntryKey x

taggingTagTaggingKey :: Tagging -> TagKey
taggingTagTaggingKey (Tagging _ x) = TagKey x

--- Dynamic predicate representing the Tagging relation between Entry entities and Tag entities
tagging :: EntryKey -> TagKey -> KeyDatabase.Dynamic
tagging (EntryKey key1) (TagKey key2) = taggingEntry unknown (key1,key2)

--- Inserts a new Tagging relation between a Entry entity and a Tag entity
newTagging :: EntryKey -> TagKey -> KeyDatabase.Transaction ()
newTagging key1 key2 =
  ERDGeneric.existsEntryWithDBKey "Entry" entryEntry (entryKeyToKey key1) |>>
   (ERDGeneric.existsEntryWithDBKey "Tag" tagEntry (tagKeyToKey key2) |>>
    (ERDGeneric.unique2 taggingEntry (entryKeyToKey key1) (tagKeyToKey key2)
     |>>
     ERDGeneric.newEntryR taggingEntry (entryKeyToKey key1)
      (tagKeyToKey key2)))

--- Deletes an existing Tagging relation between a Entry entity and a Tag entity
deleteTagging :: EntryKey -> TagKey -> KeyDatabase.Transaction ()
deleteTagging key1 key2 =
  ERDGeneric.deleteEntryR taggingEntry (entryKeyToKey key1) (tagKeyToKey key2)

--- Gets the associated Entry entities for a given Tag entity
getEntryTags :: Entry -> KeyDatabase.Transaction [Tag]
getEntryTags e =
  let ekey = entryKey e
   in KeyDatabase.getDB
       (queryCondTagging (\ t -> taggingEntryTaggingKey t == ekey)) |>>=
       (KeyDatabase.mapT getTag . map taggingTagTaggingKey)

--- Gets all Tagging relationship entities stored in the database.
queryAllTaggings :: KeyDatabase.Query [Tagging]
queryAllTaggings =
  KeyDatabase.transformQ (map (uncurry keytuple2Tagging))
   (KeyDatabase.allDBKeyInfos taggingEntry)

--- Gets all Tagging relationship entities satisfying a given condition.
queryCondTagging :: (Tagging -> Bool) -> KeyDatabase.Query [Tagging]
queryCondTagging econd =
  KeyDatabase.transformQ (filter econd) queryAllTaggings

--- Dynamic predicate representing role "tagged".
tagged :: EntryKey -> TagKey -> KeyDatabase.Dynamic
tagged = tagging

--- Dynamic predicate representing role "tags".
tags :: TagKey -> EntryKey -> KeyDatabase.Dynamic
tags = flip tagging

--- Dynamic predicate representing the Commenting relation
--- between Entry entities and Comment entities.
commenting :: EntryKey -> CommentKey -> KeyDatabase.Dynamic
commenting key1 key2
  | commentEntryCommentingKey en =:= key1 = commentEntry
                                             (commentKeyToKey key2)
                                             (comment2tuple en)
  where en free

--- Dynamic predicate representing role "isCommentedBy".
isCommentedBy :: EntryKey -> CommentKey -> KeyDatabase.Dynamic
isCommentedBy = commenting

--- Dynamic predicate representing role "isCommentedBy".
commentsOn :: CommentKey -> EntryKey -> KeyDatabase.Dynamic
commentsOn = flip isCommentedBy

--- Checks the consistency of the complete database.
checkAllData :: KeyDatabase.Transaction ()
checkAllData = checkTagging |>> (checkEntry |>> (checkComment |>> checkTag))

--- Checks the consistency of the database for Tagging entities.
checkTagging :: KeyDatabase.Transaction ()
checkTagging =
  KeyDatabase.getDB (KeyDatabase.allDBKeyInfos taggingEntry) |>>=
   (KeyDatabase.mapT_ checkTaggingEntry . map (uncurry keytuple2Tagging))

--- Checks the consistency of the database for Entry entities.
checkEntry :: KeyDatabase.Transaction ()
checkEntry =
  KeyDatabase.getDB (KeyDatabase.allDBKeyInfos entryEntry) |>>=
   (KeyDatabase.mapT_ checkEntryEntry . map (uncurry keytuple2Entry))

--- Checks the consistency of the database for Comment entities.
checkComment :: KeyDatabase.Transaction ()
checkComment =
  KeyDatabase.getDB (KeyDatabase.allDBKeyInfos commentEntry) |>>=
   (KeyDatabase.mapT_ checkCommentEntry . map (uncurry keytuple2Comment))

--- Checks the consistency of the database for Tag entities.
checkTag :: KeyDatabase.Transaction ()
checkTag =
  KeyDatabase.getDB (KeyDatabase.allDBKeyInfos tagEntry) |>>=
   (KeyDatabase.mapT_ checkTagEntry . map (uncurry keytuple2Tag))

checkTaggingEntry :: Tagging -> KeyDatabase.Transaction ()
checkTaggingEntry tagging_p =
  ERDGeneric.existsEntryWithDBKey "Entry" entryEntry
   (entryKeyToKey (taggingEntryTaggingKey tagging_p)) |>>
   (ERDGeneric.existsEntryWithDBKey "Tag" tagEntry
     (tagKeyToKey (taggingTagTaggingKey tagging_p)) |>>
    ERDGeneric.unique2C taggingEntry
     (entryKeyToKey (taggingEntryTaggingKey tagging_p))
     (tagKeyToKey (taggingTagTaggingKey tagging_p)))

checkEntryEntry :: Entry -> KeyDatabase.Transaction ()
checkEntryEntry entry_p =
  ERDGeneric.duplicateKeyTest entryEntry |>>
   ERDGeneric.uniqueC "Blog" entryEntry keytuple2Entry entryTitle entry_p

checkCommentEntry :: Comment -> KeyDatabase.Transaction ()
checkCommentEntry comment_p =
  ERDGeneric.duplicateKeyTest commentEntry |>>
   ERDGeneric.existsEntryWithDBKey "Entry" entryEntry
    (entryKeyToKey (commentEntryCommentingKey comment_p))

checkTagEntry :: Tag -> KeyDatabase.Transaction ()
checkTagEntry tag_p =
  ERDGeneric.duplicateKeyTest tagEntry |>>
   ERDGeneric.uniqueC "Blog" tagEntry keytuple2Tag tagName tag_p

--- Saves the complete database as Curry terms.
--- The first argument is the directory where the term files should be stored.
saveAllData :: String -> IO ()
saveAllData path =
  do ERDGeneric.saveDBTerms path "Entry" entryEntry keytuple2Entry
     ERDGeneric.saveDBTerms path "Comment" commentEntry keytuple2Comment
     ERDGeneric.saveDBTerms path "Tag" tagEntry keytuple2Tag
     ERDGeneric.saveDBTerms path "Tagging" taggingEntry keytuple2Tagging

--- Restore the complete database from files containing Curry terms.
--- The first argument is the directory where the term files are stored.
restoreAllData :: String -> IO ()
restoreAllData path =
  do ERDGeneric.restoreDBTerms path "Entry" entryEntry
      (entryKeyToKey . entryKey) entry2tuple
     ERDGeneric.restoreDBTerms path "Comment" commentEntry
      (commentKeyToKey . commentKey) comment2tuple
     ERDGeneric.restoreDBTerms path "Tag" tagEntry (tagKeyToKey . tagKey)
      tag2tuple
     ERDGeneric.restoreDBRelTerms path "Tagging" taggingEntry tagging2tuple
