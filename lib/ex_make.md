# ExMake Design Notes

## Overview

ExMake is a data-processing abstraction for full-text search.

Full-text search has a bunch of dependencies:

    results <- query <- index <- catalog <- params

The overall shape of the project resembles 'make' - where you assemble results
out of intermediate parts. 

At every step of the build chain, you want parameter validation and caching so
you don't have to re-generate everything from scratch.

As we go forward, other types of intermediate data structures and indexes will
be introduced.  We need a data-processing abstraction that is flexible,
pluggable and composable.

That's the idea of ExMake.

We're using an iterative design process.  `Build > Use > Learn > Repeat`  The
design is evolving rapidly. Feedback/ideas/contributions are welcome!

## Comparable Abstractions

- Make
- The Elixir Pipeline
- Monads, Functors, Applicatives
- Plug and Rake

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

## Questions

- Can we just use Plug ?
- What can we use from Monads, Functors and Applicatives?

## Ideas for Next Design Iteration

`Plug` is a great abstration for web processing:

    conn
    |> validate_user
    |> CSRF checking
    |> route_to_controller
    |> pull_values_from_database
    |> assemble_view

The `Plug` abstraction is simply a module with two callbacks:

    defmodule MyPlug do
      use Plug
      def init(args), do: initial_state
      def call(conn, args), do: updated_conn
    end

With `Plug`, everything revolves around the `conn` struct defined in
`Plug.Conn`.

Perhaps our next iteration could look like this:

A composable middleware module with three callbacks:

    defmodule MyStep do
      use ExMake.Search
      def init(args), do: initial_state
      def call(conn, args), do: updated_conn
      def handle(conn, args), do: updated_conn
    end

The data-processing semantics would involve a two-step pipeline.

The `call` phase:

    search
    |> ResultsStep.call
    |> QueryStep.call
    |> IndexStep.call
    |> CatalogStep.call
    |> ParamsStep.call

The `handle` phase:

    search
    |> ParamsStep.handle
    |> CatalogStep.handle
    |> IndexStep.handle
    |> QueryStep.handle
    |> ResultsStep.handle

The `ExMake.Search` struct would have required slots for standard search
elements, and optional slots for expansion:

    %{
      params:  {digest, data} ,
      catalog: {digest, data} ,
      index:   {digest, data} ,
      results: {digest, data}
    }

The `ExMake.Search` struct would support a number of helper functions that
could be used in the `call` and `handle` callbacks:

- set_params(obj)
- set_catalog(obj)
- validate
- cache
- digest
