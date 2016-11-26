defmodule Searchex.Search do
  def do_query({catalog, query}) do
    {catalog, String.split(query) |> Searchex.KeywordSer.do_query}
  end
end