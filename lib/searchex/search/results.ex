defmodule Searchex.Search.Results do

  @moduledoc false

  def render({catalog, results}) do
    docs   = catalog.docs
    title  = catalog_title(catalog)
    fields = String.split(catalog.params.cli_format.fields)
    data = Searchex.Build.Catalog.Scan.table_data(docs, title: title, fields: fields )
    {title, header, rows} = data
    numdocs = Enum.count(rows)
    if numdocs == 0 do
      DIO.puts "NO RESULTS"
    else
      DIO.puts TableRex.quick_render!(rows, header, title)
    end
    {catalog, results}
  end

  def filter({catalog, results}) do
    docids   = Enum.map results, &(elem(&1, 0))
    new_docs = catalog.docs
               |> Enum.filter(fn(x) -> Enum.member?(docids, x.docid) end)
               |> Enum.sort_by(fn(x) -> Enum.find_index(docids, fn(y) -> x == y end) end)
               |> Enum.slice(0, 20)
    {Map.merge(catalog, %{docs: new_docs}) , results}
  end

  defp catalog_title(catalog) do
    query = catalog.query || "TBD"
    title = catalog.params.cli_format.title || "Search Results"
    Enum.join [title, " [query='#{query}']"]
  end
end
