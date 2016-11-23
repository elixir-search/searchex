# Searchex Readme

A full-text search engine written in pure Elixir.

This application is UNDER CONSTRUCTION - not yet ready for use.

## About Searchex

Searchex provides a search capability for full-text documents.  Example
document types include:

- text, markdown, XML and JSON files
- source code files
- product descriptions
- blog and forum posts
- chat rooms and twitter feeds
- web pages

## Quick Start

TBD

## Searchex Architecture

A Searchex `DOCUMENT` has two key elements:

1. document `META-DATA`, like `title`, `author_name`, `publication_date` 

2. the `FULL-TEXT` of the document 

Searchex organizes documents into separate `COLLECTIONS`.  Each collection has
two main elements:

1. the `CATALOG`, a table-like structure that contains the document ID,
meta-data, and document location.

2. the `INDEX`, an inverted index that is built for fast search and retrieval

Each collection is defined by a `CONFIG` file, a yaml file that specifies
things like:

- document directories
- file types
- meta-data fields definitions and extraction regexes
- document separator regex (for multi-doc files)

## Using Searchex

A `searchex` command-line program can be used to manage config files, catalogs
and indexes, and to perform searches.

There is an API that Elixir developers can use to embed Searchex into their
applications.

## Why Erlang/Elixir

Text indexing and search can be incredibly compute intensive, and benefit
massively from the concurrent/distributed capabilities of the BEAM.

# Package Installation

Add `searchex` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [{:searchex, "~> 0.0.1-alpha.2"}]
end
```

Then run `mix deps.get`

## Escript Installation

If you have Elixir 1.3+ enter this at the console:

<pre><sub>mix escript.install https://raw.githubusercontent.com/elixir-search/searchex/master/searchex</sub></pre>

## Tab Completion

Get a tab-completion script by typing `searchex completion`

To install: copy this script to `/etc/bash_completion.d` (or equivalent)

     $ searchex completion > /etc/bash_completion.d/searchex_completion.bash
     $ chmod a+rx /etc/bash_completion.d/searchex_completion.bash

## Roadmap

- [x] Config management 
- [x] Porter stemming algorithm
- [x] BM25 query algorithm
- [x] Simple CLI
- [ ] Additional document examples
- [ ] Faceted Search
- [ ] Server mode
- [ ] Streaming document ingestion
- [ ] Filesystem watcher
- [ ] Alerting
- [ ] Phoenix integration
- [ ] Vim and Emacs plugins

## GitHub Source

<https://github.com/elixir-search/searchex> 
