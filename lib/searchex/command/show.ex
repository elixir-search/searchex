defmodule Searchex.Command.Show do
  @moduledoc false

  def exec(idnum) do
    results = elem(Searchex.Command.Search.Cache.read_results,0)
    docs    = results.docs
    doc     = Enum.at(docs, String.to_integer(idnum))
    body = doc.body
    X.DIO.puts body
  end
end
