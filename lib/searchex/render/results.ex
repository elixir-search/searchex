require IEx

defmodule Searchex.Render.Results do

  @moduledoc false

  def to_table({:error, _msg}) do
    Util.Ext.IO.puts "NO RESULTS"
  end

  def to_table(frame) do
    Util.Ext.IO.tins(frame.results.docs, color: "MAGENTA")
    results = frame.results
    docs    = results.docs
    title   = "Collection '#{frame.cfg_name}' Query '#{frame.query}'"
    fields  = String.split(get_fields(frame.params))
    {header, rows} = table_data(docs, title: title, fields: fields )
    if Enum.count(rows) == 0 do
      Util.Ext.IO.puts "NO RESULTS"
    else
      # Note: TableRex Bug - https://github.com/djm/table_rex/issues/13
      Util.Ext.IO.puts title
      Util.Ext.IO.puts TableRex.quick_render!(rows, header)
    end
    :ok
  end

  defp get_fields(params) do
    Map.from_struct(params)[:display_fields] || ""
  end

  @doc "Generate table data from the scan"
  def table_data(docs, opts) do
    fields  = ~w(docid) ++ opts[:fields]
    headers = Enum.map fields, &clean_header(&1)
    rows    = docs
              |> Enum.with_index(1)
              |> Enum.map(&row_data(&1, fields))
    {["ID"] ++ headers, rows}
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

  # get fields
  defp row_item(doc, <<"f:" , field_name::binary>>) do
    Map.get(doc.fields, String.to_atom(field_name)) || "TBD"
  end

  # get fields
  defp row_item(doc, header_name) do
    value = Map.get(doc, String.to_atom(header_name)) || "TBD"
    "#{value}"
    |> String.slice(0..40)
    |> String.replace(~r/[\j\n\m\r\e\a\f\t\v]/, " ")
    |> String.replace(~r/[\x80-\xff]/, "")
    |> String.slice(0..30)
  end
end