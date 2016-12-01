defmodule Searchex.Command.Search do

  @moduledoc false

  def exec(cfg_name, query) do
    DIO.puts "SEARCH #{cfg_name} #{query}"
    handle_chain({:do_query, cfg_name, query})
  end

  def handle_chain({:do_query, _cfg_name, _query}) do
    # TODO: add error checking
    # bad query
    # zero results
    # ALSO - re-checking CFGs isn't necessary - it can be done lower in the chain
#    cond do
#      cfg_name_invalid?(cfg_name) -> {:error, cfg_name_invalid_msg(cfg_name) }
#      cfg_dir_absent?(cfg_name)   -> {:error, cfg_dir_absent_msg(cfg_name)   }
#      cfg_missing?(cfg_name)      -> {:error, cfg_missing_msg(cfg_name)      }
#      cfg_invalid?(cfg_name)      -> {:error, cfg_invalid_msg(cfg_name)      }
#      true                        -> do_query(cfg_name, query)
#    end
  end

#  def handle_chain({:do_query, cfg_name, query}) do
#    %{
#      validations:       &(local_validations(cfg_name, query)),
#      chained_parents:   &(chained_parents(cfg_name)),
#      local_timestamp:   &(local_timestamp(cfg_name)),
#      action_when_fresh: &({:ok, timestamp_now}),
#      action_when_stale: &({:ok, timestamp_now})
#    }
#  end

#  defp local_validations(cfg_name, query) do
#    [
#      validation1(cfg_name)    ,
#    ]
#  end

#  defp chained_parents(cfg_name) do
#    [
#      Searchex.Command.Index.chain({:build_index, cfg_name})
#    ]
#  end

  # -----

#  defp validation1(cfg_name) do
#    cond do
#      cfg_name == nil -> {:error, "Name (#{cfg_name}) is nil"}
#      _               -> {:ok}
#    end
#  end

#  defp do_query(cfg_name, query) do
#    case chain_and_check(parents(cfg_name), timestamp_now) do
#      {:error , msg   } -> {:error, msg}
#      {:fresh , values} -> do_query_from_scratch(values)
#      {:stale , values} -> load_results_from_cache(values)
#    end
#  end

#  defp parents(cfg_name), do: Searchex.Command.Index.chain({:load_index, cfg_name})

#  defp do_query_from_scratch() do
#    catalog = Searchex.Command.Index.exec(cfg_name)
#    {catalog, query}
#    |> do_query
#    |> Searchex.Command.Search.Results.filter
#    |> Searchex.Command.Search.Cache.write_results
#  end

#  defp load_results_from_cache do
#
#  end

#  defp do_query2({catalog, query}) do
#    new_catalog = Map.merge(catalog, %{query: query})
#    {new_catalog, String.split(query) |> Searchex.Keyword.Server.do_query}
#  end
end
