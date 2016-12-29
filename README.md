# Searchex Readme

![build status](https://api.travis-ci.org/elixir-search/searchex.svg?branch=dev "Travis CI build status")

A full-text search engine written in pure Elixir.  

Three goals: 1) a CLI simple as grep, 2) an API that scales without limit, and
3) a simple way to exchange searchable document repos.

BEAM, OTP, GenStage and Flow gives us the best possible foundation on which to
build. 

## About Searchex

Searchex provides a search capability for full-text documents.  Example
document types include:

- text, markdown, XML and JSON files
- source code files
- product descriptions
- blog and forum posts
- chat rooms and twitter feeds
- web pages

Searchex allows you to create searchable Repos that can be shared over the
Internet.  There is a sample repo [on GitHub](https://github.com/elixir-search/sample).

Searchex is a new project, usable for testing but not recommended for
production.  For testing, we're using collections of up to 2,000 documents with
1MB of raw text.  See the [Roadmap](#roadmap) for development plans.

## Demo

[![asciicast](https://asciinema.org/a/26dqlxx2qf878melvc71shpgm.png)](https://asciinema.org/a/26dqlxx2qf878melvc71shpgm)

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
  [{:searchex, "~> 0.0.1"}]
end
```
Then run `mix deps.get`

View API documentation at https://hexdocs.pm/searchex

## Quick Start

After the `searchex` escript is installed...

1) Fetch a Searchex repository
 
    > searchex fetch elixir-search/sample

2) Run

    > searchex help                        # show help page
    > searchex ls                          # list collections
    > searchex info                        # show collection stats
    > searchex query tiny .                # list docs from collection: tiny
    > searchex show tiny 1                 # show the doc 1 from 'tiny'
    > searchex query genesis 'cain abel'   # query docs from collection: genesis

## Roadmap

- [x] Config management 
- [x] Porter stemming algorithm
- [x] BM25 query algorithm
- [x] Middleware framework
- [x] CLI Indexing and Query
- [x] LRU Cache
- [x] CLI Results Display
- [x] Fetchable document repos
- [ ] Server mode
- [ ] Incremental add/remove/update
- [ ] Git-based file-change detection
- [ ] Filesystem watcher
- [ ] Streaming document ingestion (GenStage/Flow)
- [ ] Faceted Search
- [ ] CLI Tab Completion
- [ ] LRU Registry
- [ ] Typeahead support
- [ ] Alerting
- [ ] Multi-collection search
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
- Sample Repository: <https://github.com/elixir-search/sample>
- StemEx: <https://github.com/elixir-search/stem_ex>
- Hex Package: <https://hex.pm/packages/searchex>
- API Documentation: <https://hexdocs.pm/searchex>
