defmodule Searchex.Cmd do
  @moduledoc """
  Run Searchex commands
  """
 
  defp catalog_int(cfg_name) do
    DIO.puts "CATALOG #{cfg_name}"
    cfg_name
    |> Searchex.Cfg.cfg_cat
    |> Searchex.Cfg.to_map
    |> Searchex.Util.Map.atomify_keys
    |> Searchex.Build.Catalog.Params.create_from_cfg
    |> Searchex.Build.Catalog.read_or_generate
  end

  def catalog(cfg_name) do
    catalog_int(cfg_name)
    {:ok}
  end

  def index(cfg_name) do
    DIO.puts "INDEX #{cfg_name}"
    catalog_int(cfg_name)
    |> Searchex.Build.Index.read_or_generate
    Searchex.Build.Index.Render.to_console
    {:ok}
  end

  def build(cfg_name) do
    DIO.puts "BUILD #{cfg_name}"
    index(cfg_name)
    {:ok}
  end

  def info(_cfg_name) do
    DIO.puts "INFO : UNDER CONSTRUCTION"
  end

  def search(cfg_name, query) do
    DIO.puts "SEARCH #{cfg_name} #{query}"
    catalog_int(cfg_name)
    |> Searchex.Build.Index.read_or_generate
    String.split(query)
    |> Searchex.KeywordSer.do_query
    {:ok}
  end

  def query(cfg_name, query) do
    search(cfg_name, query)
  end

  def results do
    DIO.puts "RESULTS : UNDER CONSTRUCTION"
  end

  def show(_docnum) do
    DIO.puts "SHOW : UNDER CONSTRUCTION"
  end

  def edit(_docnum) do
    DIO.puts "EDIT : UNDER CONSTRUCTION"
  end
end

