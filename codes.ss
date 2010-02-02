#lang scheme

(define error-codes
  #("FogBugz not initialized.  Database may be down or needs to be upgraded.Q"
    "Log On problem - Incorrect Username or Password"
    "Log On problem - multiple matches for username"
    "You are not logged on."
    "Argument is missing from query"
    "Edit problem - the case you are trying to edit could not be found"
    "Edit problem - the action you are trying to perform on this case is not permitted"
    "Time tracking problem - you can't add a time interval to this case because the case can't be found, is closed, has no estimate, or you don't have permission"
    "New case problem - you can't write to any project"
    "Case has changed since last view"
    "Search problem - an error occurred in search."
    "Wiki creation problem"
    "Wiki permission problem"
    "Wiki load error"
    "Wiki template error"
    "Wiki commit error"
    "No such project"
    "No such user"
    "Area creation problem"
    "FixFor creation problem"
    "Project creation problem"
    "User creation problem"
    "Project percent time problem"
    "No such fixfor"
    "Violates fixfor execution order"))

(define event-codes
  #("evtOpened" "evtEdited" "evtAssigned" "evtReactivated"
    "evtReopened" "evtClosed " "evtMoved" "evtUnknown"
    "evtReplied" "evtForwarded" "evtReceived" "evtSorted"
    "evtNotSorted" "evtResolved" "evtEmailed" "evtReleaseNoted"
    "evtDeletedAttachment"))

(provide (all-defined-out))
