# Searchex Readme

A full-text search engine written in pure Elixir.

The goals: a CLI simple as grep, an API that scales to the biggest search
problems. 

BEAM, OTP, GenStage and Flow gives us the best possible foundation on which to
build. 

Searchex is UNDER CONSTRUCTION - not yet ready for use.

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

2. the `INDEX`, an inverted index built for fast search and retrieval

Each collection is defined by a `CONFIG` file, a yaml file that specifies
things like:

- document directories
- file types
- meta-data fields definitions and extraction regexes
- document separator regex (for multi-doc files)

## The Searchex CLI

The `searchex` command-line program can manage config files, build catalogs and
indexes, and perform searches.

### Escript Installation

If you have Elixir 1.3+ enter this at the console:

<pre><sub>mix escript.install https://raw.githubusercontent.com/elixir-search/searchex/master/searchex</sub></pre>

Make sure `~/.mix/escripts` is on your path!

## The Searchex API

Elixir developers can embed Searchex into their applications.

Add `searchex` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [{:searchex, "~> 0.0.1-alpha.2"}]
end
```

Then run `mix deps.get`

View API documentation at https://hexdocs.pm/searchex

## Roadmap

- [x] Config management 
- [x] Porter stemming algorithm
- [x] BM25 query algorithm
- [ ] Simple CLI
- [ ] Repository for Sample Docs
- [ ] Faceted Search
- [ ] CLI Tab Completion
- [ ] Server mode
- [ ] Streaming document ingestion (GenStage/Flow)
- [ ] Filesystem watcher
- [ ] Typeahead support
- [ ] Alerting
- [ ] Configuration GUI
- [ ] Firestorm integration
- [ ] Phoenix integration
- [ ] Docsource Adapters (filesys, fn, DB, twitter, etc)
- [ ] Toolchain Integration (ExDoc, Hex, GitHub Issues)
- [ ] Community Integration (Elixir Slack, Elixir Forum)
- [ ] Searchable Tutorials (Elixir Blogs, Slide Decks, Videos)
- [ ] Vim and Emacs plugins
- [ ] Internationalization
- [ ] Dockerization
- [ ] Baysean/ML Classifiers

## Links

- Searchex Website: <http://searchex.org>
- GitHub Source: <https://github.com/elixir-search/searchex> 
- Sample Document Repository: <https://github.com/elixir-search/sample_docs>
- StemEx: <https://github.com/elixir-search/stem_ex>
- Hex Package: <https://hex.pm/packages/searchex>
- API Documentation: <https://hexdocs.pm/searchex>
