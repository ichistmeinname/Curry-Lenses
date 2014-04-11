--------------------------------------------------------------------------
--- This module implements the management to execute user processes.
--------------------------------------------------------------------------

module Processes(
  processNames,
  isInProcess, startProcess, removeCurrentProcess, advanceInProcess,
  nextControllerRefInProcessOrForUrl
 ) where

import UserProcesses

import Global
import Maybe
import HTML

import Routes
import RoutesData
import Session

import ReadShowTerm
import AllSolutions

--------------------------------------------------------------------------
-- A operations on the datatype for process systems.

processesOf  (ProcSpec procs _ _) = procs

--- The names of the processes in a process system.
processNames :: Processes _ -> [String]
processNames (ProcSpec procs _ _) = map fst procs

getControllerForState :: stid -> Processes stid -> ControllerReference
getControllerForState sid (ProcSpec _ ctrlof _) = ctrlof sid

--- Is a state a final state, i.e., without successors, in a process system?
isFinalState sid (ProcSpec _ _ trans) = do
  succs <- getOneSolution (\x -> let p free in x =:= trans sid p)
  return (maybe True (const False) succs)

--------------------------------------------------------------------------
-- The current processes are stored in a persistent entity.
currentProcess :: Global (SessionStore String)
currentProcess = global emptySessionStore Temporary

--- Returns the process state stored in the user session.
getCurrentProcess :: IO (Maybe _)
getCurrentProcess = do
  curProc <- getSessionData currentProcess
  case curProc of
    Just sids -> return $ Just (readQTerm sids)
    Nothing -> return Nothing

--- Is the current user session in a process interaction?
isInProcess :: IO Bool
isInProcess =
  getSessionData currentProcess >>= return . maybe False (const True)

--- Saves the state of a process, i.e., a node in the process graph,
--- in the user session.
saveCurrentProcess :: _ -> IO ()
saveCurrentProcess sid = putSessionData (showQTerm sid) currentProcess

--- Deletes the process in the user session.
removeCurrentProcess :: IO ()
removeCurrentProcess = removeSessionData currentProcess

--- Starts a new process with a given name. In the next step, the
--- controller of the start state of the process is executed.
startProcess :: String -> IO [HtmlExp]
startProcess pname =
  maybe (return [htxt $ "startProcess: process not found: " ++ pname])
        (\state -> do saveCurrentProcess state
                      return [htxt ""] -- triggers redirect
        )
        (lookup pname (processesOf availableProcesses))

--- Advance by one state in the process description.
--- The parameter contains some optional data from the controllers
--- in order to influence the next transition.
advanceInProcess :: Maybe ControllerResult -> IO ()
advanceInProcess ctrlinfo = do
  curprocess <- getCurrentProcess
  case curprocess of
    Nothing  -> done -- no active process, do nothing
    Just sid -> do
     let (ProcSpec _ _ trans) = availableProcesses
     nextsid <- getOneValue (trans sid ctrlinfo)
     case nextsid of
        Just sid' -> saveCurrentProcess sid'
        Nothing   -> done -- this case should not occur in a good spec.

--- Returns the next controller in the current process or,
--- if there is no current process, the controller associated to the given URL.
nextControllerRefInProcessOrForUrl :: String
                                   -> IO (Maybe ControllerReference)
nextControllerRefInProcessOrForUrl url = do
  curprocess <- getCurrentProcess
  case curprocess of
    Nothing  -> getControllerReference url -- no current process
    Just sid -> do isfinal <- isFinalState sid availableProcesses
                   if isfinal then removeCurrentProcess
                              else done
                   return (Just (getControllerForState sid availableProcesses))
