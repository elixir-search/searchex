defmodule Searchex.Command.Index do

  use ExMake

  import Searchex.Config.Helpers
  import Searchex.Command.Build.Index

  def exec(cfg_name) do
    DIO.inspect "DING EXEC", color: "RED"
    chain({:load_index, cfg_name})
  end

  @doc false
  def handle_chain({:load_index, cfg_name}) do
    DIO.inspect "BANG HANDLE", color: "RED"
    %{
      validations:       []                                                               ,
      children:          fn -> children(cfg_name)                                    end  ,
      lcl_timestamp:     fn -> lcl_timestamp(cfg_name)                               end  ,
      action_when_fresh: fn(state) -> load_index_from_cache(state, cfg_name)         end  ,
      action_when_stale: fn(state) -> generate_index_from_scratch(state, cfg_name)   end  ,
    }
  end

  # -----

  defp children(cfg_name) do
    [
      Searchex.Command.Catalog.chain({:load_catalog, cfg_name})
    ]
  end

  defp load_index_from_cache(state, cfg_name) do
    DIO.inspect "INDEX FRESH", color: "BLUE"
    [catalog | _] = state
    start_supervisor(:index)
    Searchex.Command.Build.Index.Cache.read_index(catalog)
    {:ok, lcl_timestamp(cfg_name), catalog}
  end

  defp generate_index_from_scratch(state, cfg_name) do
    DIO.inspect "INDEX STALE", color: "BLUE"
    start_supervisor(:index)
    [catalog | _rest] = state
    catalog |> create_from_catalog
    Searchex.Command.Build.Index.Cache.write_index(catalog)
    {:ok, lcl_timestamp(cfg_name), catalog}
  end

  defp lcl_timestamp(cfg_name) do
    DIO.inspect [PATH: idx_file(cfg_name)], color: "YELLOW"
    DIO.inspect(idx_file(cfg_name) |> filepath_timestamp, color: "YELLOW")
  end
end