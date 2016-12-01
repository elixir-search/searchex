defmodule Searchex.Command.Build.Catalog.Cache do

  @moduledoc false

  import Searchex.Config.Helpers

  @doc "Write catalog to output file"
  def write_catalog(scan) do
    path = cache_file(scan)
    string = :erlang.term_to_binary(scan)
    File.write(path, string)
    scan
  end

  @doc "Read catalog from input file"
  def read_catalog(params) do
    path = cache_file(params)
    {:ok, string} = File.read(path)
    :erlang.binary_to_term(string)
  end

  # -----

  defp cache_file(scan) do
    cfg_name = Map.get(scan, :collection) || scan.params.collection
    cat_file(cfg_name)
  end
end
