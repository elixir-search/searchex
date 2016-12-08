defmodule Searchex.Command.Index do

  @moduledoc false

  use ExMake

  import Searchex.Command.Build.Index

  def exec(cfg_name) do
    chain({:load_index, cfg_name})
  end

  def chain_children({:load_index, cfg_name}) do
    start_supervisor(:index)
    [ Searchex.Command.Catalog.chain({:load_catalog, cfg_name}) ]
  end

#  def chain_restore({:load_index, _cfg_name}, cached_state) do
#    {map, digest} = cached_state
#    map_to_otp(map)
#    digest
#  end

  def chain_generate({:load_index, _cfg_name}, child_state) do
    [{catalog, _digest} | _rest] = child_state
    catalog |> create_from_catalog
    catalog
  end
end