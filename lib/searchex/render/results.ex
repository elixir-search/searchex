require IEx

defmodule Searchex.Render.Results do

  @moduledoc false

  def to_table({:error, _msg}) do
    DIO.puts "NO RESULTS"
  end

  def to_table({catalog, results}) do
    docs   = catalog.docs
    title  = catalog_title(catalog)
    fields = String.split(catalog.params.cli_format.fields)
    data = Searchex.Command.Build.Catalog.Filescan.table_data(docs, title: title, fields: fields )
    {title, header, rows} = data
    numdocs = Enum.count(rows)
    if numdocs == 0 do
      DIO.puts "NO RESULTS"
    else
      DIO.puts TableRex.quick_render!(rows, header, title)
    end
    {catalog, results}
  end

  defp catalog_title(catalog) do
    query = Map.from_struct(catalog)[:query] || "TBD"
    title = catalog.params.cli_format[:title] || "Search Results"
    Enum.join [title, " [query='#{query}']"]
  end
end