--------------------------------------------------------------------------
--- This module contains the definition of the processes that can
--- be selected by the user.
--------------------------------------------------------------------------

module UserProcesses(Processes(..),
                     ControllerResult,
                     availableProcesses)
 where

import RoutesData

--------------------------------------------------------------------------
--- Datatype for the specification of process systems.
--- Such a specification consists of a list of process names together
--- with their start state, a mapping from states into controller
--- function references, and a (non-deterministic) operation
--- that maps a state and some information from the controller
--- into some new state. If in some situation there is more
--- than one new state, the implementation picks non-deterministically
--- one of these states as the next state in the process.
--- The datatype is parameterized over the type of state identifiers (stid).
data Processes stid = ProcSpec [(String,stid)]
                               (stid -> ControllerReference)
                               (stid -> Maybe ControllerResult -> stid)

--- The type of data that is optional passed from the controllers to
--- influence the next process state (compare the call to nextInProcessOr
--- in the controllers).
type ControllerResult = String

--------------------------------------------------------------------------
-- example process:

-- Node ids for the processes:
data Node = NewEntry | NewComment | EndCmt | NewTag | EndTag

availableProcesses :: Processes Node
availableProcesses = ProcSpec [] failed failed -- default declaration

{-
-- An example of a process specification with two starting points.
availableProcesses = ProcSpec
   [("Erstellung eines Eintrags und Kommentaren",NewEntry),
    ("Neues Tag eingeben und Entry-Liste zeigen",NewTag)]
   controllerOf
   next
 where
   controllerOf :: Node -> ControllerReference
   controllerOf NewEntry   = NewEntryController
   controllerOf NewComment = NewCommentController
   controllerOf EndCmt     = ListCommentController
   controllerOf NewTag     = NewTagController
   controllerOf EndTag     = ListEntryController

   next :: Node -> Maybe ControllerResult -> Node
   next NewEntry   _        = NewComment
   next NewComment Nothing  = NewComment
   next NewComment (Just s) = if s=="finished" then EndCmt else NewComment
   next NewTag     _        = EndTag
-}


