defmodule Searchex.Search do
  def do_query({catalog, query}) do
    new_catalog = Map.merge(catalog, %{query: query})
    {new_catalog, String.split(query) |> Searchex.KeywordSer.do_query}
  end
end