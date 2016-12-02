defmodule Searchex.Command.Index do

  @moduledoc false

  use ExMake

  import Searchex.Config.Helpers
  import Searchex.Command.Build.Index

  def exec(cfg_name) do
    DIO.inspect "DING EXEC", color: "RED"
    chain({:load_index, cfg_name})
  end

  def chain_validations({:load_index, _cfg_name}), do: []

  def chain_children({:load_index, cfg_name}) do
    [ Searchex.Command.Catalog.chain({:load_catalog, cfg_name}) ]
  end

  def chain_action_when_fresh({:load_index, cfg_name}, child_state) do
    DIO.inspect "INDEX FRESH", color: "BLUE"
    [catalog | _] = child_state
    start_supervisor(:index)
    Searchex.Command.Build.Index.Cache.read_index(catalog)
    {:ok, chain_lcl_timestamp(cfg_name), catalog}
  end

  def chain_action_when_stale({:load_index, cfg_name}, child_state) do
    DIO.inspect "INDEX STALE", color: "BLUE"
    start_supervisor(:index)
    [catalog | _rest] = child_state
    catalog |> create_from_catalog
    Searchex.Command.Build.Index.Cache.write_index(catalog)
    {:ok, chain_lcl_timestamp({:load_index, cfg_name}), catalog}
  end

  def chain_lcl_timestamp({:load_index, cfg_name}) do
    idx_file(cfg_name) |> filepath_timestamp
  end
end