defmodule Searchex.Command.Index do

  @moduledoc false

  use ExMake

  import Searchex.Config.Helpers
  import Searchex.Command.Build.Index

  def exec(cfg_name) do
    chain({:load_index, cfg_name})
  end

  def chain_validations({:load_index, _cfg_name}), do: []

  def chain_children({:load_index, cfg_name}) do
    [ Searchex.Command.Catalog.chain({:load_catalog, cfg_name}) ]
  end

  def chain_action_when_fresh({:load_index, cfg_name}, child_state) do
    [catalog | _] = child_state
    DIO.inspect :FRESH_INDEX, color: "green"
    start_supervisor(:index)
    Searchex.Command.Build.Index.Cache.read_index(catalog)
    {:ok, chain_lcl_timestamp(cfg_name), catalog}
  end

  def chain_action_when_stale({:load_index, cfg_name}, child_state) do
    DIO.inspect :STALE_INDEX, color: "green"
    start_supervisor(:index)
    [catalog | _rest] = child_state
    catalog |> create_from_catalog
    Searchex.Command.Build.Index.Cache.write_index(catalog)
    {:ok, chain_lcl_timestamp({:load_index, cfg_name}), catalog}
  end

  def chain_lcl_timestamp({:load_index, cfg_name}) do
    cfg_name |> idx_file |> filepath_timestamp
  end
end