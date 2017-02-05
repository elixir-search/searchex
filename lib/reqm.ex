defmodule Reqm do
  @moduledoc """
  Reqm (REQuest Middleware) middlware is a Plug-like framework
  optimized for search requests.

  Full-text search is build on layers of dependencies:

      results <- query <- index <- catalog <- params

  The data-processing requirements resembles 'make' - where you assemble
  results out of intermediate parts.

  At every step of the build chain, you want parameter validation and caching
  so you don't have to re-generate everything from scratch.

  Custom middleware modules can be added to handle different languages or
  custom indexing requirements.  Reqm middleware is flexible and composable.

  That's the idea of Reqm.

  ## Inspired by Plug

  The architecture of Reqm is inspired by Plug.  Here are some differences
  between Reqm and Plug...

  ### Terminology

  | Plug         | Reqm        |
  |--------------|--------------|
  | Plug.Builder | Reqm.Module |
  | Plug.Conn    | Reqm.Frame  |
  | plug         | step         |
  | pipeline     | job          |
  | conn         | frame        |
  | call         | call         |

  ### The Reqm Frame

  Plug is centered around a the Conn struct.  Reqm uses the Frame struct.

  See the documentation for `Reqm.Frame` for more info.

  ### Search Focus

  Reqm omits Plug's HTTP-oriented helpers, and adds search-specific helpers:

  - validate - a method to perform data validations
  - digest - to create content digests
  - cache - a LRU cache for Elixir terms

  Going forward, we'll add helpers to accomodate different types of
  intermediate caches, document and configuration sources.

  ## Use Modes

  Right now `Searchex` is a CLI so everything works in 'batch processing' mode.
  Single-collection operations on small datasets.

  At some point we'll add 'server mode' where the catalogs and indexes stay
  resident in memory (or JIT retrieved from disk) and handle live updates.

  In 'server mode', we want the ability to handle thousands of gigabyte-sized
  collections running distributed across a cluster.

  ## Search Architecture

  There are two big data structures:

  1) The Catalog is like a table. The key field is 'docid' (the content
  digest).  Other fields are document location (filepath, byte offset, doc
  length) and the various fields that are extracted in the catalog process.

  2) The Index is organized in a three-level tree.  1) all the stemmed words in
  the collection for each word, 2) a list of each document that contains the
  word for each document, 3) a list of positions that the document occurred.

  For a document database of 1GB, the catalog would typically be 100MB, and the
  index typically 1GB.

  ## Data Characterization

  Data comes in two forms:

  - DataTerm - any Elixir term: map, list, atom, etc.
  - ProcessTree - a supervisor and all it's children

  Data may be in one of three states:
  - Active - in-memory ready to use
  - Cache - in-memory hot standby
  - Backup - on-disk survives executable restart

  |        | DataTerm  | ProcessTree       |
  |--------|-----------|-------------------|
  | Active | variable  | PID / ProcessName |
  | Cached | LRU Cache | LRU Registry(TBD) |
  | Backup | DETS      | DETS(?)           |

  Right now we have a nice LRU-Cache based on ETS and DETS.  Going forward we
  will need a LRU-Registry based on ETS and the Process Registry in Elixir 1.4.

  ## Work in Progress

  We're using an iterative design process.  `Build > Use > Learn > Repeat`  The
  design is evolving rapidly. Feedback/ideas/contributions are welcome!
  """

  @type opts :: binary | tuple | atom | integer | float | [opts] | %{opts => opts}

  @callback init(opts) :: opts
  @callback call(Reqm.Job.t, opts) :: Reqm.Job.t
end
