#|
 This file is a part of file-notify
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.file-notify)

(docs:define-docs
  (type failure
    "Erro signalled when an OS failure occurs.")
  
  (function init
    "Initialises the library for use.

You should call this function before anything else. It is safe to call
this function an arbitrary number of times, initialisation will only
occur once.

See SHUTDOWN")
  
  (function shutdown
    "Cleans up and shuts down the library.

All active watches will be cleared after this call.

After calling this you should not call any other library functions
unless you also call INIT again beforehand.

See INIT")
  
  (function watch
    "Adds one or more files to the watch list.

FILE/S may either be a single pathname, or a list of pathnames for
files to be watched. If a given file does not exist or is not
accessible, an error may be signalled.

EVENTS should be T or a list of events to watch. Supported event types
are:

  :ACCESS        --- [LINUX] The file was accessed.
  :ALL           --- [ALL] All available events are watched.
  :ATTRIBUTE     --- [ALL] The file attributes changed.
  :CLOSE         --- [LINUX] A file was closed.
  :CLOSE-NOWRITE --- [LINUX] A file was closed without writing.
  :CLOSE-WRITE   --- [LINUX] A file was closed after writing.
  :CREATE        --- [ALL] A file was created.
  :DELETE        --- [ALL] A file was deleted.
  :DELETE-SELF   --- [LINUX] The file was deleted.
  :MODIFY        --- [ALL] The file was modified.
  :MOVE          --- [ALL] A move or rename occurred.
  :MOVE-SELF     --- [LINUX] The file was moved.
  :MOVED-FROM    --- [LINUX] A file was moved from elsewhere.
  :MOVED-TO      --- [LINUX] A file was moved elsewhere.
  :OPEN          --- [LINUX] The file was opened.
  :RENAME-FILE   --- [WINDOWS] A file was renamed, created, or deleted.
  :RENAME-DIRECTORY --- [WINDOWS] A directory was renamed, created, or deleted.
  :SECURITY      --- [WINDOWS] A security descriptor changed
  :SIZE          --- [WINDOWS] The size of the file changed

It is safe to call WATCH on the same file multiple times. Subsequent
watches will be ignored, and the watch will only happen once.

When a directory is watched, events for files contained within the
directory (subject to the above event filters) will be watched.

Note that it is not guaranteed that the filename you passed will
actually be watched. The underlying system may not accurately support
the capabilities you asked and may upgrade the events watched or even
the files being watched.

This will call INIT for you.

See UNWATCH
See LIST-WATCHED
See PROCESS-EVENTS
See INIT")
  
  (function list-watched
    "Returns the list of files the user passed to WATCH.

See WATCH")
  
  (function unwatch
    "Removes one or more files from the watch list.

FILE/S may either be a single pathname, or a list of pathnames for
files to be unwatched. If a given pathname was not watched, it is
ignored.

Note that, depending on other files being watched, you may still
receive events for a file even after you've explicitly unwatched it.

See WATCH
See LIST-WATCHED")
  
  (function process-events
    "Processes available file change events.

FUNCTION is called with two arguments -- the path of the file changed,
and the type of change event that occurred. The function may be called
multiple times.

TIMEOUT may be either T, NIL, or a real in [0, infty[.
When T, events will be continuously and indefinitely received. When
NIL, events will only be processed if immediately available. When a
real, the function will wait up to at most that number of seconds to
receive events.

The events received may be of types that were not explicitly requested
in the WATCH command, and for files that were not explicitly
watched. It is up to you to only look at the files and event types you
care about.

The TYPE passed to the function may be one of the following:

  :ACCESS        --- The file was accessed somehow.
  :ATTRIBUTE     --- The file's attributes were changed.
  :CLOSE-NOWRITE --- The file was closed without writing.
  :CLOSE-WRITE   --- The file was closed and written to.
  :CREATE        --- The file was created.
  :DELETE        --- The file was deleted.
  :MODIFY        --- The file was modified somehow.
  :MOVE          --- The file was moved.
  :MOVED-FROM    --- A file was moved and this is the old name.
  :MOVED-TO      --- A file was moved and this is the new name.
  :OPEN          --- The file was opened.

See WATCH
See UNWATCH
See WITH-EVENTS
See NORMALIZE-EVENT")
  
  (function with-events
    "Shorthand macro to process file change events.

See PROCESS-EVENTS"))
