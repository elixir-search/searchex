  defmodule Searchex.Command.Build.Catalog.Render do

#    @moduledoc false
#
#    def to_console(scan) do
#      case Mix.env do
#        :test -> scan
#        _     -> print(scan)
#      end
#      scan
#    end

    # -----

#    defp print(scan) do
#    {title, header, rows} = Searchex.Command.Build.Catalog.Filescan.table_data(scan, headers: ~w(doclength filename body f:date))
#    numdocs = Enum.count(scan.docs)
#    DIO.puts """
#
#    NUMDIOCS: #{numdocs}
#    """
#    unless numdocs == 0, do: IO.puts TableRex.quick_render!(rows, header, title)
#    scan
#    end
  end
