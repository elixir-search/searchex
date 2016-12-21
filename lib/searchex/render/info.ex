defmodule Searchex.Render.Info do

  @moduledoc false

  def to_table(vlist) do
    if length(vlist) == 0 do
      Util.Ext.IO.puts "NO COLLECTIONS"
    else
      fields = ~w(repo/collection numdocs doc_size cache_size)
      {header, rows} = table_data(vlist, fields)
#      Util.Ext.IO.puts TableRex.quick_render!(rows, header)
      Util.Ext.IO.puts table_render(rows, header)
    end
    :ok
  end

  defp table_render(rows, header) do
    alias TableRex.Table
    Table.new(rows, header)
    |> Table.put_column_meta(1..3, align: :right)
    |> Table.put_header_meta(0..3, align: :center)
    |> Table.render!
  end

  defp table_data(vlist, fields) do
    rows = vlist
           |> Enum.sort_by(fn(elem) -> elem[:cfg_name] end)
           |> Enum.map(fn(elem) -> [elem[:cfg_name], elem[:numdocs], elem[:doc_size], elem[:cache_size]] end)
    {fields, rows}
  end
end