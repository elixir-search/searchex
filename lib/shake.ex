defmodule Shake do
  @moduledoc """
  Shake is a data-processing framework optimized for search.

  Full-text search has a bunch of dependencies:

      results <- query <- index <- catalog <- params

  The overall shape of the project resembles 'make' - where you assemble results
  out of intermediate parts.

  At every step of the build chain, you want parameter validation and caching so
  you don't have to re-generate everything from scratch.

  As we go forward, other types of intermediate data structures and indexes will
  be introduced.  We need a data-processing abstraction that is flexible,
  pluggable and composable.

  That's the idea of Shake.

  ## Inspired by Plug

  The architecture of Shake is inspired by Plug.  Here are some differences between
  Shake and Plug...

  ### Terminology

  | Plug         | Shake        |
  |--------------|--------------|
  | Plug.Builder | Shake.Module |
  | Plug.Conn    | Shake.Frame  |
  | plug         | step         |
  | pipeline     | job          |
  | conn         | frame        |
  | call         | call         |

  ### The Shake Frame

  Plug is centered around a the Conn struct.  Shake uses the Frame struct.

        defstruct cfg_name: "",
                  params:   %{},
                  catalog:  %{},
                  index:    %{},
                  query:    "",
                  results:  %{},
                  halted:   false,
                  halt_msg: "",
                  digests:  %{},
                  assigns:  %{}

  ### Search Focus

  Shake omits Plug's HTTP-oriented helpers, and adds search-specific helpers:

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
  the collection for each word, 2) a list of each document that contains the word
  for each document, 3) a list of positions that the document occurred.

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

  |        | DataTerm             | ProcessTree            |
  |--------|----------------------|------------------------|
  | Active | variable             | PID / ProcessName      |
  | Cached | ETS-based LRU Cached | ETS-based LRU Cache(?) |
  | Backup | DETS                 | DETS(?)                |

  ## Work in Progress

  We're using an iterative design process.  `Build > Use > Learn > Repeat`  The
  design is evolving rapidly. Feedback/ideas/contributions are welcome!
  """

  @type opts :: binary | tuple | atom | integer | float | [opts] | %{opts => opts}

  @callback init(opts) :: opts
  @callback call(Shake.Job.t, opts) :: Shake.Job.t
end