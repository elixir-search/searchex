defmodule Searchex.Command.Search do

  @moduledoc false

  # TODO: add error checking
  # bad query
  # zero results
  # ...

  def exec(cfg_name, query) do
    DIO.puts "SEARCH #{cfg_name} #{query}"
    catalog = Searchex.Command.Index.exec(cfg_name)
    {catalog, query}
    |> do_query
    |> Searchex.Command.Search.Results.filter
    |> Searchex.Command.Search.Cache.write_results
  end

  defp do_query({catalog, query}) do
    new_catalog = Map.merge(catalog, %{query: query})
    {new_catalog, String.split(query) |> Searchex.Keyword.Server.do_query}
  end
end
