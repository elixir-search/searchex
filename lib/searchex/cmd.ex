defmodule Searchex.Cmd do
  @moduledoc """
  Run Searchex commands
  """

  defp catalog_int(cfg_name) do
    DIO.puts "CATALOG #{cfg_name}"
    Searchex.Build.Catalog.assemble(cfg_name)
  end

  def catalog(cfg_name) do
    catalog_int(cfg_name)
    {:ok}
  end

  defp index_int(cfg_name) do
    DIO.puts "INDEX #{cfg_name}"
    catalog = catalog_int(cfg_name)
    Searchex.Build.Index.read_or_generate(catalog)
    catalog
  end

  def index(cfg_name) do
    index_int(cfg_name)
    {:ok}
  end

  def build_int(cfg_name) do
    DIO.puts "BUILD #{cfg_name}"
    index_int(cfg_name)
  end

  def build(cfg_name) do
    build_int(cfg_name)
    {:ok}
  end

  def info(_cfg_name) do
    {:ok, "INFO: UNDER CONSTRUCTION"}
  end

  defp search_int(cfg_name, query) do
    DIO.puts "SEARCH #{cfg_name} #{query}"
    {index_int(cfg_name), query}
    |> Searchex.Search.do_query
    |> Searchex.Search.Cache.write_results
    |> Searchex.Search.Results.render
  end

  def search(cfg_name, query) do
    search_int(cfg_name, query)
    {:ok}
  end

  def query(cfg_name, query) do
    search(cfg_name, query)
  end

  def results do
    {:ok, "RESULTS : UNDER CONSTRUCTION"}
  end

  def show(_docnum) do
    {:ok, "SHOW : UNDER CONSTRUCTION"}
  end

  def edit(_docnum) do
    {:ok, "EDIT : UNDER CONSTRUCTION"}
  end
end

