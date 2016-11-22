defmodule Searchex.Cmd do
  @moduledoc """
  Run Searchex commands
  """
 
  use GenServer

  alias Searchex.Util.IO, as: DO

  defp catalog_int(cfg_name) do
    DO.puts "CATALOG #{cfg_name}"
    cfg_name
    |> Searchex.Cfg.cat
    |> Searchex.Cfg.to_map
    |> Searchex.Build.Catalog.Params.create_from_cfg
    |> Searchex.Build.Catalog.read_or_generate
  end

  def catalog(cfg_name) do
    catalog_int(cfg_name)
    {:ok}
  end

  def index(cfg_name) do
    DO.puts "INDEX #{cfg_name}"
    catalog_int(cfg_name)
    |> Searchex.Build.Index.read_or_generate
    Searchex.Build.Index.Render.to_console
    {:ok}
  end

  def build(cfg_name) do
    DO.puts "BUILD #{cfg_name}"
    index(cfg_name)
    {:ok}
  end

  def search(cfg_name, query) do
    DO.puts "SEARCH #{cfg_name} #{query}"
    catalog_int(cfg_name)
    |> Searchex.Build.Index.read_or_generate
    String.split(query)
    |> Searchex.KeywordSer.do_query
    {:ok}
  end
end

