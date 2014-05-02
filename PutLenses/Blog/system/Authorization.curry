--- This module specifies the access authorization to web pages.

module Authorization(AccessType(..), AccessResult(..),
                     checkAuthorization) where

import Spicey(Controller,displayError)
import HTML

--- The various kinds of operations applied to an entity.
data AccessType a = NewEntity | ShowEntity a | UpdateEntity a | DeleteEntity a
                  | ListEntities

--- The result of checking an authorization. The access is either granted
--- or denied with a string explaining the reason.
data AccessResult = AccessGranted | AccessDenied String

--- Checks the results of an authoriation access.
--- If the access is granted, we proceed with the given controller,
--- otherwise we display the access error message.
checkAuthorization :: IO AccessResult -> Controller -> Controller
checkAuthorization getaccess controller = do
  accresult <- getaccess
  case accresult of
    AccessGranted       -> controller
    AccessDenied reason -> displayError reason

entryOperationAllowed :: AccessType _ -> IO AccessResult
entryOperationAllowed at =
  case at of
    DeleteEntity _ -> return (AccessDenied "Operation not allowed!")
    _ -> return AccessGranted
