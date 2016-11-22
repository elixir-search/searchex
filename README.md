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

<pre><sub>mix escript.install https://raw.githubusercontent.com/andyl/stemex/master/stemex</sub></pre>

## Tab Completion

Get a tab-completion script by typing `searchex completion`

To install: copy this script to `/etc/bash_completion.d` (or equivalent)

     $ searchex completion > /etc/bash_completion.d/searchex_completion.bash
     $ chmod a+rx /etc/bash_completion.d/searchex_completion.bash

## Comparables and Reference Docs

* <http://blog.equanimity.nl/blog/2011/10/07/a-basic-full-text-search-server-in-erlang/>

* <http://stackoverflow.com/questions/195809/is-it-possible-to-develop-a-powerful-web-search-engine-using-erlang-mnesia-ya>

* <https://github.com/tmaciejewski/see>

* <http://himmele.blogspot.com/2011/04/build-your-own-internet-search-engine.html>

## Roadmap

- [x] Stemming algorithm
- [x] Implement BM25 query altorithm
- [_] Faceted Search
- [_] Additional document examples

## GitHub Source

<https://github.com/andyl/searchex> 
