module RoutesData where

import Authentication

data ControllerReference
 = ProcessListController | LoginController | ListEntryController 
 | NewEntryController | ListCommentController | NewCommentController 
 | ListTagController | NewTagController 
data UrlMatch = Exact String | Matcher (String -> Bool) | Always 
type Route = (String,UrlMatch,ControllerReference)

--- This constant specifies the association of URLs to controllers.
--- Controllers are identified here by constants of type
--- ControllerReference. The actual mapping of these constants
--- into the controller operations is specified in the module
--- ControllerMapping.
getRoutes :: IO [Route]
getRoutes =
  do login <- getSessionLogin
     return
      [("Processes",Exact "spiceyProcesses",ProcessListController)
      ,("List Entry",Exact "listEntry",ListEntryController)
      ,("New Entry",Exact "newEntry",NewEntryController)
      ,("List Comment",Exact "listComment",ListCommentController)
      ,("New Comment",Exact "newComment",NewCommentController)
      ,("List Tag",Exact "listTag",ListTagController)
      ,("New Tag",Exact "newTag",NewTagController)
      ,(maybe "Login" (const "Logout") login,Exact "login",LoginController)
      ,("default",Always,ListEntryController)]
