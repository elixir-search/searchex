defmodule Searchex.Cmd do
  @moduledoc """
  Run Searchex commands
  """

  def catalog(cfg_name) do
    DIO.puts "CATALOG #{cfg_name}"
    Searchex.Build.Catalog.assemble(cfg_name)
  end

  def index(cfg_name) do
    DIO.puts "INDEX #{cfg_name}"
    catalog = catalog(cfg_name)
    Searchex.Build.Index.read_or_generate(catalog)
    catalog
  end

  def build(cfg_name) do
    DIO.puts "BUILD #{cfg_name}"
    index(cfg_name)
  end

  def info(_cfg_name) do
    {:ok, "INFO: UNDER CONSTRUCTION"}
  end

  def search(cfg_name, query) do
    DIO.puts "SEARCH #{cfg_name} #{query}"
    {index(cfg_name), query}
    |> Searchex.Search.do_query
    |> Searchex.Search.Cache.write_results
    |> Searchex.Search.Results.render
  end

  def query(cfg_name, query) do
    search(cfg_name, query)
  end

  def results do
    Searchex.Search.Cache.read_results
    |> Searchex.Search.Results.render
  end

  def show(_docnum) do
    {:ok, "SHOW : UNDER CONSTRUCTION"}
  end

  def edit(_docnum) do
    {:ok, "EDIT : UNDER CONSTRUCTION"}
  end
end

