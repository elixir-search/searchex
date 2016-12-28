defmodule Searchex.Render.Results do

  @moduledoc false

  def to_table({:error, _msg}) do
    Util.Ext.IO.puts "NO RESULTS"
  end

  def to_table(frame) do
    results = frame.results
    docs    = results.docs
    if docs == nil || Enum.count(docs) == 0 do
      Util.Ext.IO.puts "NO RESULTS"
    else
      title  = "Collection '#{frame.cfg_name}' Query '#{frame.query}'"
      fields = String.split(get_fields(frame.params))
      {header, rows} = table_data(docs, title: title, fields: fields )
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
    |> String.replace("i:", "")
    |> String.replace("f:", "")
    |> String.capitalize
  end

  defp row_data({doc, idx}, headers) do
    [idx] ++ Enum.map headers, &row_item(doc, &1)
  end

  # get fields
  defp row_item(doc, <<"i:", field_name::binary>>) do
    Map.get(doc.fields, String.to_atom(field_name)) || "TBD"
  end

  defp row_item(doc, <<"f:", field_name::binary>>) do
    funcmap(doc, field_name).()
  end

  # get fields
  defp row_item(doc, header_name) do
    value = Map.get(doc, String.to_atom(header_name)) || "TBD"
    "#{value}"
    |> String.slice(0..40)
    |> String.replace(~r/[\j\n\r\e\a\f\t\v]/, " ")
    |> String.replace(~r/[\x80-\xff]/, "")
    |> String.slice(0..30)
  end

  defp funcmap(doc, field) do
    %{
      "filename1" => fn() -> Map.get(doc, :filename) |> join_path(1) end     ,
      "filename2" => fn() -> Map.get(doc, :filename) |> join_path(2) end     ,
      "filename3" => fn() -> Map.get(doc, :filename) |> join_path(3) end     ,
      "filename4" => fn() -> Map.get(doc, :filename) |> join_path(4) end     ,
      "filename5" => fn() -> Map.get(doc, :filename) |> join_path(5) end     ,
    }[field]
  end

  defp join_path(string, depth) do
    string
    |> String.split("/")
    |> Enum.reverse
    |> Enum.take(depth)
    |> Enum.reverse
    |> Enum.join("/")
  end
end