module TagController (
 newTagController, editTagController, deleteTagController, listTagController
 ) where

import Spicey
import KeyDatabase
import HTML
import Time
import Blog
import TagView
import Maybe
import Authorization
import AuthorizedControllers
import UserProcesses

--- Shows a form to create a new Tag entity.
newTagController :: Controller
newTagController =
  checkAuthorization (tagOperationAllowed NewEntity) $
   (do return (blankTagView createTagController))

--- Persists a new Tag entity to the database.
createTagController :: Bool -> String -> Controller
createTagController False _ = listTagController
createTagController True name =
  do transResult <- runT (newTag name)
     either (\ _ -> nextInProcessOr listTagController Nothing)
      (\ error -> displayError (showTError error)) transResult

--- Shows a form to edit the given Tag entity.
editTagController :: Tag -> Controller
editTagController tagToEdit =
  checkAuthorization (tagOperationAllowed (UpdateEntity tagToEdit)) $
   (do return (editTagView tagToEdit updateTagController))

--- Persists modifications of a given Tag entity to the
--- database depending on the Boolean argument. If the Boolean argument
--- is False, nothing is changed.
updateTagController :: Bool -> Tag -> Controller
updateTagController False _ = listTagController
updateTagController True tag =
  do transResult <- runT (updateTag tag)
     either (\ _ -> nextInProcessOr listTagController Nothing)
      (\ error -> displayError (showTError error)) transResult

--- Deletes a given Tag entity (depending on the Boolean
--- argument) and proceeds with the list controller.
deleteTagController :: Tag -> Bool -> Controller
deleteTagController _ False = listTagController
deleteTagController tag True =
  checkAuthorization (tagOperationAllowed (DeleteEntity tag)) $
   (do transResult <- runT (deleteTag tag)
       either (\ _ -> listTagController)
        (\ error -> displayError (showTError error)) transResult)

--- Lists all Tag entities with buttons to show, delete,
--- or edit an entity.
listTagController :: Controller
listTagController =
  checkAuthorization (tagOperationAllowed ListEntities) $
   (do tags <- runQ queryAllTags
       return
        (listTagView tags showTagController editTagController
          deleteTagController))

--- Shows a Tag entity.
showTagController :: Tag -> Controller
showTagController tag =
  checkAuthorization (tagOperationAllowed (ShowEntity tag)) $
   (do return (showTagView tag listTagController))
