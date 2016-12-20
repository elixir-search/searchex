defmodule Searchex.Command do
  @moduledoc """
  Main Searchex workflow

  1. Read Configuration
  2. Build Catalog
  3. Build Index
  4. Perform Query

  This workflow establishes a dependency chain, with higher level steps
  depending on the outputs of lower level steps.  Each step generates an
  intermediate output which can be cached to minimize re-execution of
  compute-intensive steps.  The command structure is based on `Shake`.
  """

  alias Util.Cache

  @doc """
  Generate the catalog for `cfg_snip`

  The catalog is a Map that contains all configuration data, document text and meta-data.

  The catalog is generated from a config file, stored at `~/.Searchex.Configs/<cfg_snip>.yml`.

  The catalog is cached on disk at `~/.searchex/data/<cfg_snip>_cat.dat`.
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

  @doc false
  @altdoc """
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
  Query the collection
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

  @doc false
  # Show document text
  def show(cfg_snip, tgt_id) do
    Searchex.Command.Show.exec(cfg_snip, tgt_id)
  end

  @doc false
  @nodoc """
  Removed all cached files.
  """
  def clean(cfg_snip) do
    frame = Searchex.Command.Params.exec(cfg_snip)
    file  = Searchex.Command.CmdHelpers.cache_file(frame)
    if File.exists?(file), do: File.rm!(file)
    {:ok}
  end

end
