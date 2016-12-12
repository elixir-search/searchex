defmodule Searchex.Command.Query do

  @moduledoc false

  use Shake.Module

  @doc "Module API"
  def exec(cfg_name, query) do
    X.DIO.puts "QUERY #{cfg_name} #{query}"
    call(%Frame{cfg_name: cfg_name, query: query}, [])
  end

#  def chain_children({:do_search, cfg_name, _query}) do
#    [ Searchex.Command.Index.chain({:load_index, cfg_name}) ]
#  end

  step Searchex.Command.Index
  step :do_query

#  def chain_action_when_fresh({:do_search, cfg_name, query}, _child_state) do
#    X.DIO.inspect :FRESH_SEARCH, color: "green"
#    state = Searchex.Command.Search.Cache.read_results
#    {:ok, chain_lcl_timestamp({:do_search, cfg_name, query}), state}
#  end

  def do_query(%Frame{cfg_name: cfg_name, query: query} = frame, _opts) do
    results = {cfg_name, String.split(query)}
            |> Searchex.Keyword.Server.do_query
    %Frame{frame | results: results}
  end

#  def chain_generate({:do_search, cfg_name, query}, child_state) do
#    [catalog | _rest] = child_state
#    state = {cfg_name, String.split(query)}
#            |> Searchex.Keyword.Server.do_query
##            |> Searchex.Command.Search.Results.filter
##            |> add_query(query)
#            |> Searchex.Command.Search.Cache.write_results
#    {catalog, state}
#  end

#  defp add_query({collection, result}, query) do
#    {Map.merge(collection, %{query: query}), result}
#  end
end
