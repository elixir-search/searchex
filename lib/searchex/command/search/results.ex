defmodule Searchex.Command.Search.Results do

  @moduledoc false

  def filter({catalog, results}) do
    DIO.inspect "INFILTER", color: "RED"
    docids   = Enum.map results, &(elem(&1, 0))
    new_docs = catalog.docs
               |> Enum.filter(fn(x) -> Enum.member?(docids, x.docid) end)
               |> Enum.sort_by(fn(x) -> Enum.find_index(docids, fn(y) -> x == y end) end)
               |> Enum.slice(0, 20)
    {Map.merge(catalog, %{docs: new_docs}) , results}
  end
end
