defmodule Searchex.Render.Help do

  @moduledoc false

  def to_table(cmd_opts) do
    {:ok, cfgs} = Searchex.Config.ls
    title       = String.upcase(prog()) <> " Version #{prog_version()} - FOR TESTING ONLY"
    header      = ~w(Command Arguments Description)
    rows        = table_data(cmd_opts)
    footer      = "Collections: " <> Enum.join(cfgs, ", ")
    Util.Ext.IO.puts table_render(rows, header, title)
    Util.Ext.IO.puts footer
    :ok
  end

  defp table_data(cmd_opts) do
    cmd_opts
    |> Enum.map(fn({cmd,_,_,args,detail}) -> [cmd, args, detail] end)
  end

  defp table_render(rows, header, title) do
    alias TableRex.Table
    Table.new(rows, header, title)
    |> Table.render!
  end

  defp prog         , do: Util.Ext.App.name

  defp prog_version , do: Util.Ext.App.version
end