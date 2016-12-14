require IEx

defmodule Searchex.Render.Results do

  @moduledoc false

  def to_table({:error, _msg}) do
    Util.Ext.IO.puts "NO RESULTS"
  end

  def to_table(frame) do
    catalog = frame.catalog
    results = frame.results
    docs    = catalog.docs
    title   = catalog_title(frame)
    fields  = String.split(get_fields(frame.params))
    data = table_data(docs, title: title, fields: fields )
    {title, header, rows} = data
    numdocs = Enum.count(rows)
    if numdocs == 0 do
      Util.Ext.IO.puts "NO RESULTS"
    else
      Util.Ext.IO.puts TableRex.quick_render!(rows, header, title)
    end
    {catalog, results}
  end

  defp get_fields(params) do
    case params.cli_format do
      nil -> ""
      map -> map[:fields] || ""
    end
  end

  defp catalog_title(frame) do
    query = frame.query || "TBD"
    title = frame.params.cli_format[:title] || "Search Results"
    Enum.join [title, " [query='#{query}']"]
  end

  @doc "Generate table data from the scan"
  def table_data(docs, opts \\ [title: "Collection", fields: ~w(filename doclength docstart body)]) do
    fields  = ~w(docid) ++ opts[:fields]
    headers = Enum.map fields, &clean_header(&1)
    rows    = docs
              |> Enum.with_index(1)
              |> Enum.map(&row_data(&1, fields))
    {opts[:title], ["ID"] ++ headers, rows}
  end

  # ---------------------------------------------------------------------------------------------

  defp clean_header(header) do
    header
    |> String.replace("f:", "")
    |> String.capitalize
  end

  defp row_data({doc, idx}, headers) do
    [idx] ++ Enum.map headers, &row_item(doc, &1)
  end

  defp row_item(doc, <<"f:" , field_name::binary>>) do
    Map.get(doc.fields, String.to_atom(field_name)) || "TBD"
  end

  defp row_item(doc, header_name) do
    value = Map.get(doc, String.to_atom(header_name)) || "TBD"
    "#{value}"
    |> String.slice(0..40)
    |> String.replace(~r/[\j\n\m\r\e\a\f\t\v]/, " ")
    |> String.replace(~r/[\x80-\xff]/, "")
    |> String.slice(0..30)
  end
end