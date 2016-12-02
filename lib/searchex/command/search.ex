defmodule Searchex.Command.Search do

  @moduledoc false

#  use ExMake
#
  def exec(cfg_name, query) do
    DIO.puts "SEARCH #{cfg_name} #{query}"
    chain({:do_query, cfg_name, query})
  end


#
#    defp children(cfg_name) do
#      [
#        Searchex.Command.Catalog.chain({:load_index, cfg_name})
#      ]
#    end
#
#    defp action_when_stale(args, child_state) do
#      DIO.inspect "STALE ------------------------", color: "YELLOW"
#      DIO.inspect args, color: "green"
#      DIO.inspect child_state, color: "blue"
#
#      #    new_catalog = Map.merge(catalog, %{query: query})
#      #    {new_catalog, String.split(query) |> Searchex.Keyword.Server.do_query}
#
#      #    catalog = Searchex.Command.Index.exec(cfg_name)
#      #    {catalog, query}
#      #    |> do_query
#      #    |> Searchex.Command.Search.Results.filter
#      #    |> Searchex.Command.Search.Cache.write_results
#
#      {:ok, timestamp_now}
#    end
#
#    defp action_when_fresh(args, child_state) do
#      DIO.inspect "FRESH ------------------------", color: "YELLOW"
#      DIO.inspect args, color: "green"
#      DIO.inspect child_state, color: "blue"
#      {:ok, timestamp_now}
#    end
#
#  defp lcl_timestamp(cfg_name) do
#    idx_file(cfg_name) |> filepath_timestamp
#  end
end
