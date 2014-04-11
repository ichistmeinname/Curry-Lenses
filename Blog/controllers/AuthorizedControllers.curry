module AuthorizedControllers where

import Authorization
import Blog

--- Checks whether the application of an operation to a Entry
--- entity is allowed.
entryOperationAllowed :: AccessType Entry -> IO AccessResult
entryOperationAllowed at =
  case at of
   ListEntities -> return AccessGranted
   NewEntity -> return AccessGranted
   (ShowEntity _) -> return AccessGranted
   (DeleteEntity _) -> return AccessGranted
   (UpdateEntity _) -> return AccessGranted

--- Checks whether the application of an operation to a Comment
--- entity is allowed.
commentOperationAllowed :: AccessType Comment -> IO AccessResult
commentOperationAllowed at =
  case at of
   ListEntities -> return AccessGranted
   NewEntity -> return AccessGranted
   (ShowEntity _) -> return AccessGranted
   (DeleteEntity _) -> return AccessGranted
   (UpdateEntity _) -> return AccessGranted

--- Checks whether the application of an operation to a Tag
--- entity is allowed.
tagOperationAllowed :: AccessType Tag -> IO AccessResult
tagOperationAllowed at =
  case at of
   ListEntities -> return AccessGranted
   NewEntity -> return AccessGranted
   (ShowEntity _) -> return AccessGranted
   (DeleteEntity _) -> return AccessGranted
   (UpdateEntity _) -> return AccessGranted
