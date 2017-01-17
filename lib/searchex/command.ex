defmodule Searchex.Command do
  @moduledoc """
  Main Searchex workflow

      results <- query <- index <- catalog <- params <- docsrc

  This workflow establishes a dependency chain, with higher level steps
  depending on the outputs of lower level steps.  Each step generates an
  intermediate output which can be cached to minimize re-execution of
  compute-intensive steps.  The processing middleware is based on `Shake`.

  An LRU Cache is used, with auto-expiration of old keys.  Cache keys are
  digests of the content produced at each step of the build chain.

  Note that all of these functions take a `cfg_snip` argument.  The `cfg_snip`
  is a wildcard string which matches against the repo/collection name.  For
  example, if the repo/collection name is `sample/genesis`, any of these
  cfg_snips would match (`sample/genesis`, `genesis`, `sampgeni`, `geni`)

  All of these functions return simple Elixir terms with no output formatting.
  See `Searchex.Render` for a list of functions that perform special handling
  on command output.
  """

  alias Util.Cache

  @doc """
  Generate the catalog for `cfg_snip`

  The catalog is a Map that contains all configuration data, document text and meta-data.

  The catalog is generated from a config file, stored at `~/.searchex/repo/<cfg_snip>.yml`.
  """
  def catalog(cfg_snip) do
    Searchex.Command.Catalog.exec(cfg_snip) |> Cache.save
  end

  @doc """
  Generate the index for `cfg_snip`

  The index is a data structure used for fast search and retrieval.

  The index lives in a Process Tree, one worker for each keyword.
  """
  def index(cfg_snip) do
    Searchex.Command.Index.exec(cfg_snip) |> Cache.save
  end

  @doc """
  Generate both the catalog and the index for `cfg_snip` in one step
  """
  def build(cfg_snip) do
    Searchex.Command.Build.exec(cfg_snip) |> Cache.save
  end

  @doc """
  Return info about the collection
  - Number of documents
  - Generation date
  - Average size of documents
  - etc.
  """
  def info(cfg_snip) do
    Searchex.Command.Info.exec(cfg_snip)
  end

  @doc """
  Query the collection, and return query scores.
  """
  def query(cfg_snip, query) do
    Searchex.Command.Query.exec(cfg_snip, query) |> Cache.save
  end

  @doc """
  Show last results
  """
  def results(cfg_snip) do
    Searchex.Command.Results.exec(cfg_snip)
  end

  @doc """
  Show document text
  """
  def show(cfg_snip, tgt_id) do
    Searchex.Command.Show.exec(cfg_snip, tgt_id)
  end
end
