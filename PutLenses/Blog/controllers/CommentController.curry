module CommentController (
 newCommentController, editCommentController, deleteCommentController,
 listCommentController
 ) where

import Spicey
import KeyDatabase
import HTML
import Time
import Blog
import CommentView
import Maybe
import Authorization
import AuthorizedControllers
import UserProcesses

--- Shows a form to create a new Comment entity.
newCommentController :: Controller
newCommentController =
  checkAuthorization (commentOperationAllowed NewEntity) $
   (do allEntrys <- runQ queryAllEntrys
       ctime <- getLocalTime
       return (blankCommentView ctime allEntrys createCommentController))

--- Persists a new Comment entity to the database.
createCommentController
 :: Bool -> (String,String,CalendarTime,Entry) -> Controller
createCommentController False _ = listCommentController
createCommentController True (text ,author ,date ,entry) =
  do transResult <- runT
                     (newCommentWithEntryCommentingKey text author date
                       (entryKey entry))
     either (\ _ -> nextInProcessOr listCommentController Nothing)
      (\ error -> displayError (showTError error)) transResult

--- Shows a form to edit the given Comment entity.
editCommentController :: Comment -> Controller
editCommentController commentToEdit =
  checkAuthorization (commentOperationAllowed (UpdateEntity commentToEdit)) $
   (do allEntrys <- runQ queryAllEntrys
       commentingEntry <- runJustT (getCommentingEntry commentToEdit)
       return
        (editCommentView commentToEdit commentingEntry allEntrys
          updateCommentController))

--- Persists modifications of a given Comment entity to the
--- database depending on the Boolean argument. If the Boolean argument
--- is False, nothing is changed.
updateCommentController :: Bool -> Comment -> Controller
updateCommentController False _ = listCommentController
updateCommentController True comment =
  do transResult <- runT (updateComment comment)
     either (\ _ -> nextInProcessOr listCommentController Nothing)
      (\ error -> displayError (showTError error)) transResult

--- Deletes a given Comment entity (depending on the Boolean
--- argument) and proceeds with the list controller.
deleteCommentController :: Comment -> Bool -> Controller
deleteCommentController _ False = listCommentController
deleteCommentController comment True =
  checkAuthorization (commentOperationAllowed (DeleteEntity comment)) $
   (do transResult <- runT (deleteComment comment)
       either (\ _ -> listCommentController)
        (\ error -> displayError (showTError error)) transResult)

--- Lists all Comment entities with buttons to show, delete,
--- or edit an entity.
listCommentController :: Controller
listCommentController =
  checkAuthorization (commentOperationAllowed ListEntities) $
   (do comments <- runQ queryAllComments
       return
        (listCommentView comments showCommentController editCommentController
          deleteCommentController))

--- Shows a Comment entity.
showCommentController :: Comment -> Controller
showCommentController comment =
  checkAuthorization (commentOperationAllowed (ShowEntity comment)) $
   (do commentingEntry <- runJustT (getCommentingEntry comment)
       return (showCommentView comment commentingEntry listCommentController))

--- Gets the associated Entry entity for a given Comment entity.
getCommentingEntry :: Comment -> Transaction Entry
getCommentingEntry cEntry = getEntry (commentEntryCommentingKey cEntry)
