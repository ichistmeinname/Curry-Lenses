module ControllerMapping where

import Spicey
import Routes
import SpiceySystemController
import RoutesData
import EntryController
import CommentController
import TagController

--- Maps the controllers associated to URLs in module RoutesData
--- into the actual controller operations.
getController :: ControllerReference -> Controller
getController fktref =
  case fktref of
   ProcessListController -> processListController
   LoginController -> loginController
   ListEntryController -> listEntryController
   NewEntryController -> newEntryController
   ListCommentController -> listCommentController
   NewCommentController -> newCommentController
   ListTagController -> listTagController
   NewTagController -> newTagController
   _ -> displayError "getController: no mapping found"
