This package adds the ability to sync org-mode tasks with
Toodledo, a powerful web-based todo list manager that welcomes 3rd
party integrations.  (See http://www.toodledo.com/)

This version of `org-toodledo' utilizes version 2.0 of the Toodledo API.

SYNCHRONIZING FOR THE FIRST TIME
--------------------------------

The first step in using org-toodledo is to initialize a file and
synchronize tasks.  Simply create a new file, change the mode to
`org-mode', then call `org-toodledo-initialize'.  This will create
a new heading called "TASKS" (by default) and will import all
non-deleted tasks from Toodledo as sub-headings beneath "TASKS".

If you already have an existing list of tasks in org file, open the
org file first.  Move the cursor to the headling where you want
imported tasks from Toodledo to be inserted into the buffer.  Call
`org-toodledo-initialize'.  This will import all tasks from the
server as well as pushing existing tasks in the org file back to
the server.

Once an org-file has been initialized, the heading selected will
be given a few Toodledo specific properties that are used to track
the status of synchronization:

  * TASKS
    :PROPERTIES:
    :ToodledoLastSync: 1315343842
    :ToodledoLastEdit: 1315337478
    :ToodledoLastDelete: 1314972230
    :OrgToodledoVersion: 2.3
    :END:

This is referred to as the 'base Toodledo entry'.

SYNCHRONIZING TASKS
-------------------

The local org-file can be synchronized with the server at any time
by calling `org-toodledo-sync'.  When called, the following steps
are performed:

  1. Tasks added to the server since the last sync are downloaded
     and inserted as sub-headings to the Toodledo base heading (has
     the `ToodledoLastSync' property)

  2. Tasks modified on the server are compared against the local
     copy.  If the local copy was not modified since the last sync,
     the local copy is updated.  If local copy was modified, the
     server copy is inserted *after* the local copy as a duplicate.
     The user must manually merge any changes

  3. Tasks deleted on the server are removed entirely from the
     local org file.

  4. Tasks modified locally are pushed to the server as edits.

  5. Tasks created and not yet prseent on the server are pushed as
     new tasks.

  6. Tasks marked for deletion are deleted from the server, and
     then purged from the local file.

Changes to tasks are automatically detected by computing a hash of
the task fields.  This hash is computed and saved as a property of
the task on sync.  When the next sync occurs, the hash value is
compared and if it differs, the task is considered modified.  This
eliminates the need for the user to mark tasks as modified or
remembere which tasks have changed -- it's all automatic!

Note that `org-toodledo-sync' scans the entire file for tasks, not
just subheadings of the base entry.

ADDING NEW TASKS
----------------

To add a new task on the server, just create a new headline
anywhere in the org file and give the headline a TODO keyword.
When ready, call `org-toodledo-sync' to push new tasks to the
server.

DELETING TASKS
--------------

Tasks cannot simply be killed from the org-file like text if the
were already synced with the server since they will just come back
the next time `org-toodledo-sync' is called.  Instead, they must be
marked as deleted by calling `org-toodledo-mark-task-deleted'.  Call
this function from any point within the task.  At the next sync,
the task will be deleted from the server and then killed from the
local file.

Note that it may not be necessary to delete tasks in this way.  Instead
complete the task and let Toodledo archive completed tasks.

TOODLEDO FIELDS
---------------

The table lists the possible Toodledo fields and how they are
mapped to org-mode style tasks:

| Toodledo Field | Org-mode               | Comments                                     |
| id             | Property :ToodledoID:  | If present, this task was previoiusly synced |
| title          | Heading                | Heading minus TODO state, priority and tags  |
| status         | TODO state             | See `org-toodledo-status-to-org-map'         |
| startdate      | SCHEDULED              | startdate/startime are GMT                   |
| starttime      | SCHEDULED              |                                              |
| duedate        | DEADLINE               | duedate/duetime are GMT                      |
| duetime        | DEADLINE               |                                              |
| completed      | CLOSED                 | Timestamp when the task was marked completed |
| repeat         | Repeat interval        |                                              |
| repeatfrom     |                        |                                              |
| context        | Tag                    | Context string "Work" becomes a tag :@Work:  |
| modified       | Property :Modified:    | Timestamp when last modifed (set by server)  |
| folder         | Property :Folder:      |                                              |
| goal           | Property :Goal:        |                                              |
| priority       | Priority               | 3=>A, 2=>B, 1=>C, -1,0 => D                  |
| note           | Body                   | Body of the task minus the properties        |
| length         | Effort                 |                                              |
| parent         |                        | Links tasks parent/child                     |
| tag            | Tag                    | org-mode tags, note context is also a tag    |

TODO STATES
-----------

The TODO states from Toodledo are mapped to org-mode states via the
`org-toodledo-status-to-org-map' alist.   This can be customized to
choose your own TODO states, but all 10 states from Toodledo should
be mapped, even if only a subset are used in org-mode.

In order to cycle through all the states recognized by Toodledo,
put a line like the following somewhere in your org file:

  #+SEQ_TODO: TODO(t) DELEGATED(g) SOMEDAY(s) WAITING(w) | DONE(d) CANCELLED(c) REFERENCE(r)

CONTEXTS
--------

Toodledo 'Contexts' allow you to split tasks into contexts such as
Work and Home.  Contexts are mapped to org tags with the '@' keyword,
:@Work: and :@Home:.

Currently only contexts already on the server are recognized.  Setting
the task context of :@Phone: when Phone is not a valid context will
loose the context.

SUBTASKS
--------

Sub-tasks are supported by Toodledo with a Pro account subscription.
When enabled, a 2-level task hierarchy is supported:

  * TODO Write a best-selling novel
  ** DONE Make an outline
  ** WAITING Call Susan about the contract
  ** TODO Finish writing
  ** TODO Profit

The parent/child relationship is tracked dynamically at the time
of sync, looking for the next heading up for each task, and if present
and a task, link the task to the parent.

Bi-directional synchronization is fully supported.

If the account is not a Pro account, subtasks will still be synced
to the server, but the parent/child relationship is not.  This
yields a flat list of tasks on the server.  Note that the hierarchy
in the org file is still maintained even though not on the server.

NOTE: A hierarchy of TODO items of more than 2 levels is not supported
by the server.  If 3 or more levels is present, all children will
appear directly beneath the top-most TODO item:

  org-mode:
     * TODO Level 1 item
     ** WAITING Level 1.1 item
     *** DONE Level 1.1.1 item
     ** DONE Level 1.2 item
     *** DONE Level 1.2.1 item

  server:
     * TODO Level 1 item
     ** WAITING Level 1.1 item
     ** DONE Level 1.1.1 item
     ** DONE Level 1.2 item
     ** DONE Level 1.2.1 item

Note that the hierarchy is preserved in the org-mode file, it just
displays with the children flattened on the server.

MISCELLANEOUS NOTES
-------------------

 - Doesn't do lots of error trapping. Might be a good idea to
   version-control your Org file.

 - Verify handling of other tags that are not context

 - The body of a task is stored as the Toodledo note.  May get
   confused by asterisks, so don't use any starting asterisks in
   your body text.  (or anything that looks like an Org headline).

 - w3mexcerpt.el inlcudes things needed things from w3m (since w3m
   requires things which require things which require things which
   require an executable which is no longer readily
   available.). (sachac)

 - By default, save will ask to sync with Toodledo.  This can
   behavior can be changed via `org-toodledo-sync-on-save'.

FUTURE WORK
-----------

** TODO Feature Requests: highest priority at top

[ ] access to toodledo via proxy would also be good for those
    inside proxy based firewalls. (stophlong)

[ ] Add a 'purge-completed-tasks' function -- once these tasks have
    been synced to the server, kill them locally (since they are
    backed up on toodledo).  Alternatively, move them to an archive
    file.  (cjwhite)

[ ] Option to restrict synchronization to just sync tasks under the
    the base Toodledo entry.  (cjwhite)

[ ] Support tasks across all agenda files.  (cjwhite)
