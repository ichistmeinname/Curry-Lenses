module EntryController (
 newEntryController, editEntryController, deleteEntryController,
 listEntryController
 ) where

import Spicey
import KeyDatabase
import HTML
import Time
import Blog
import EntryView
import Maybe
import Authorization
import AuthorizedControllers
import UserProcesses

--- Shows a form to create a new Entry entity.
newEntryController :: Controller
newEntryController =
  checkAuthorization (entryOperationAllowed NewEntity) $
   (do allTags <- runQ queryAllTags
       ctime <- getLocalTime
       return (blankEntryView ctime allTags createEntryController))

--- Persists a new Entry entity to the database.
createEntryController
 :: Bool -> (String,String,String,CalendarTime,[Tag]) -> Controller
createEntryController False _ = listEntryController
createEntryController True (title ,text ,author ,date ,tags) =
  do transResult <- runT
                     (newEntry title text author date |>>= addTagging tags)
     either (\ _ -> nextInProcessOr listEntryController Nothing)
      (\ error -> displayError (showTError error)) transResult

--- Shows a form to edit the given Entry entity.
editEntryController :: Entry -> Controller
editEntryController entryToEdit =
  checkAuthorization (entryOperationAllowed (UpdateEntity entryToEdit)) $
   (do allTags <- runQ queryAllTags
       taggingTags <- runJustT (getEntryTags entryToEdit)
       return
        (editEntryView (entryToEdit,taggingTags) allTags
          updateEntryController))

--- Persists modifications of a given Entry entity to the
--- database depending on the Boolean argument. If the Boolean argument
--- is False, nothing is changed.
updateEntryController :: Bool -> (Entry,[Tag]) -> Controller
updateEntryController False _ = listEntryController
updateEntryController True (entry ,tagsTagging) =
  do transResult <- runT
                     (updateEntry entry |>>
                      (getEntryTags entry |>>=
                        (\ oldTaggingTags -> removeTagging oldTaggingTags
                                              entry) |>>
                       addTagging tagsTagging entry))
     either (\ _ -> nextInProcessOr listEntryController Nothing)
      (\ error -> displayError (showTError error)) transResult

--- Deletes a given Entry entity (depending on the Boolean
--- argument) and proceeds with the list controller.
deleteEntryController :: Entry -> Bool -> Controller
deleteEntryController _ False = listEntryController
deleteEntryController entry True =
  checkAuthorization (entryOperationAllowed (DeleteEntity entry)) $
   (do transResult <- runT
                       (getEntryTags entry |>>=
                         (\ oldTaggingTags -> removeTagging oldTaggingTags
                                               entry) |>> deleteEntry entry)
       either (\ _ -> listEntryController)
        (\ error -> displayError (showTError error)) transResult)

--- Lists all Entry entities with buttons to show, delete,
--- or edit an entity.
listEntryController :: Controller
listEntryController =
  checkAuthorization (entryOperationAllowed ListEntities) $
   (do entrys <- runQ queryAllEntrys
       return
        (listEntryView entrys showEntryController editEntryController
          deleteEntryController))

--- Shows a Entry entity.
showEntryController :: Entry -> Controller
showEntryController entry =
  checkAuthorization (entryOperationAllowed (ShowEntity entry)) $
   (do taggingTags <- runJustT (getEntryTags entry)
       return (showEntryView entry taggingTags listEntryController))

--- Associates given entities with the Entry entity.
addTagging :: [Tag] -> Entry -> Transaction ()
addTagging tags entry =
  mapT_ (\ t -> newTagging (entryKey entry) (tagKey t)) tags

--- Removes association to the given entities with the Entry entity.
removeTagging :: [Tag] -> Entry -> Transaction ()
removeTagging tags entry =
  mapT_ (\ t -> deleteTagging (entryKey entry) (tagKey t)) tags
