defmodule Searchex.Command.Search do

  @moduledoc false

  use ExMake

  def exec(cfg_name, query) do
    X.DIO.puts "SEARCH #{cfg_name} #{query}"
    chain({:do_search, cfg_name, query})
  end

  def chain_children({:do_search, cfg_name, _query}) do
    [ Searchex.Command.Index.chain({:load_index, cfg_name}) ]
  end

#  def chain_action_when_fresh({:do_search, cfg_name, query}, _child_state) do
#    X.DIO.inspect :FRESH_SEARCH, color: "green"
#    state = Searchex.Command.Search.Cache.read_results
#    {:ok, chain_lcl_timestamp({:do_search, cfg_name, query}), state}
#  end

  def chain_generate({:do_search, cfg_name, query}, child_state) do
    [catalog | _rest] = child_state
    state = {cfg_name, String.split(query)}
            |> Searchex.Keyword.Server.do_query
#            |> Searchex.Command.Search.Results.filter
#            |> add_query(query)
            |> Searchex.Command.Search.Cache.write_results
    {catalog, state}
  end

#  defp add_query({collection, result}, query) do
#    {Map.merge(collection, %{query: query}), result}
#  end
end
