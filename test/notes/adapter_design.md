# Adapter Middleware Design

## Overview

The adapter connects Searchex to a document source.

With an adapter, we can index documents from the filesystem, from a database or
some other source.

## The EventLog

Source of Truth

    [
      EventID: [{create: BucketID}, {update: BucketID}, {delete: BucketID}],
      EventID: [{create: BucketID}, {update: BucketID}, {delete: BucketID}],
    ]

An event log entry has a key (EventID) and a value (the commands).
Commands can be only: create, update, delete

EventID is a sequential ID assigned by the Adapter(?).
BucketID is a unique ID - a foreign-key.

Within the bucket, there can be one or more docs.

    Repo -> Collection -> Bucket -> Doc

## Value of the EventLog

### EventID as Cache Key

Right now in the Query Processing middleware we're using content digest as a
cache key.

Generating the digest can be slow.  Not sure how you'd generate a digest of a
database...

Proposal: Use EventID instead of Digest for CacheKey

### Streaming Foundation

Building 'stream-oriented' vs 'crud-oriented' gives advantages for working in
stream-oriented infrastructures. (Kafka, Social-Media Scanning, etc.)

### Other Benefits

- possibility for P2P streaming
- simplified backup & recovery
- easier sync in distributed operations
- better sets the stage for big-data, analytics and ML

### Distributed Operations

## Adapter Types

| Adapter       | BucketID             |
|---------------|----------------------|
| FileSys       | FileName             |
| FileWatch     | FileName             |
| Archive       | ArchiveName/FileName |
| Ecto          | Type/RecordID        |
| Amqp/Rabbitmq | Channel/MsgID        |
| Couchdb       |                      |
| Kafka         |                      |
| Ejabberd      |                      |
| MongooseIM    |                      |
| Slack         |                      |
| Twitter       |                      |
| Facebook      |                      |
| Searchex      | (P2P)                |

## Config File

    :adapter:
      :type:        :filesys
      :file-types:  ["doc", "md", "txt"]
      :file-maxnum: 100

    :adapter:
      :type:  :ecto
      :tbd:   :tbd

## Adapter Behavior

### Basics

- State: params, last_event_id
- API
  - cursor -> {:digest, value} | {:lastevent, value}
  - events(after: EventID, maxnum: count, before: EventID) -> eventlog
  - rawdata(BucketID) -> string
  - validate(frame) -> :ok | {:error, [messages]}
  - init(frame)

The adapter is called from various steps in the indexing middleware:

| Function | Caller Step |
|----------|-------------|
| validate | docsource   |
| init     | docsource   |
| cursor   | catalog     |
| events   | catalog     |
| rawdata  | catalog     |

If an incremental event log can not be generated, use a :digest cursor.  This
will force the catalog and all upstream steps to be rebuilt from scratch.  Only
usable for small datasets.

### OTP

- Adapter runs as GenServer
- Start Registry and Supervisor at program init
- One adapter per collection
- Adapter process key like {:adapter, :collection_name}

## Processing Direction

Sometimes processing is initiated by Searchex, sometimes by the Source.

    SOURCE ------- ADAPTER ------ SEARCHEX

For example:

**Filesys** - updates come SEARCHEX, initiated by user action
- query processed by the command middleware 
- command middleware invokes `Searchex.Adapter.Type.pull`
- Searchex incremental-updates the index
- query results are generated

**Filewatch** - updates come from a FS_WATCHER, initated by filesystem changes
- FS_WATCHER event triggers `Searchex.Adapter.Type.pull` 
- Searchex invokes `Searchex.Adapter.Type.pull`
- Searchex incremental-updates the index
- when queries come, assume that the indexes are all up to date

## Long Term

Misc
- Adapters can be chained
- Searchex itself is an adapter -> P2P document streaming
- Docsources can be nested

    Database <- Archive <- Document

    SearchexP2P <- SearchexP2P <- Database <- Archive <- Document

3rd Party Adapters
- Must be possible for 3rd paries to write adapters
- The comparable to study is Mix Tasks
-- Mix expects tasks under the `Mix.Task` namespace
-- Searchex expects adapters under the `Searchex.Adapter` namespace

## Questions

Q: How to make a file system look like an event log?
A: Put documents under Git control.

Q: Does this mean that the Indexes will need to support snapshots?
A: Maybe.  Probably.

## Actions

### Adapters

- filesys adapter
-- test validations
-- directory checksum: `find <path> -maxdepth N -type d -ls | md4sum`
-- complete rebuild on change

- filegit adapter
-- validations
-- git-controlled directories
-- generate event log with create/update/delete commands

- filewatch adapter
-- validations
-- git controlled directories
-- file watcher - copy from 

- ecto adapter
-- validations
-- pull on callback after database transaction

### Other Changes

- rename 'filescan' to 'bucketscan'
- use EventID instead of digest
- use process tree for catalog
- incremental doc add/remove
- start the proocess registry at startup
