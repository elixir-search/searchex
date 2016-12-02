defmodule Searchex.Command.Search do

  @moduledoc false

  use ExMake

  def exec(cfg_name, query) do
    DIO.puts "SEARCH #{cfg_name} #{query}"
    chain({:do_search, cfg_name, query})
  end

  def chain_validations({:do_search, _cfg_name, _query}), do: []

  def chain_children({:do_search, cfg_name, _query}) do
    [ Searchex.Command.Index.chain({:load_index, cfg_name}) ]
  end

  def chain_action_when_fresh({:do_search, cfg_name, query}, _child_state) do
    DIO.inspect :FRESH_SEARCH, color: "green"
#    [catalog | _] = child_state
    state = Searchex.Command.Search.Cache.read_results
    {:ok, chain_lcl_timestamp({:do_search, cfg_name, query}), state}
  end

  def chain_action_when_stale({:do_search, cfg_name, query}, child_state) do
    DIO.inspect :STALE_SEARCH, color: "green"
    [catalog | _rest] = child_state
    state = {catalog, String.split(query)}
    |> Searchex.Keyword.Server.do_query
    |> Searchex.Command.Search.Results.filter
    |> Searchex.Command.Search.Cache.write_results
    {:ok, chain_lcl_timestamp({:do_search, cfg_name, query}), state}
  end

  def chain_lcl_timestamp({:do_search, _cfg_name, _query}) do
    {{0,0,0},{0,0,0}}
  end
end
