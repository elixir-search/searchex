defmodule Searchex.Command.Build.Catalog.Filescan do
  @moduledoc false

  defstruct rawdata:           ""                               ,
            input_filename:    ""                               ,
            docsep_positions:  []                               ,
            docsep_offsets:    []                               

  def generate_filescan(filename, params) do
    %Searchex.Command.Build.Catalog.Filescan{input_filename: filename}
    |> read_rawdata(params.max_file_kb)
    |> gen_docsep_positions(params)
    |> gen_docsep_offsets
  end
  
  defp read_rawdata(scan, max_file_kb) do
    rawdata = File.stream!(scan.input_filename, [], max_file_kb * 1024) |> Enum.at(0)
    %Searchex.Command.Build.Catalog.Filescan{scan | rawdata: rawdata}
  end

  defp gen_docsep_positions(scan, params) do
    positions = if params.docsep do
                  params.docsep
                  |> Regex.scan(scan.rawdata, return: :index )
                  |> Enum.map(fn(x) -> [{beg, fin} | _tail] = x; beg + fin end)
                else
                  []
                end
    %Searchex.Command.Build.Catalog.Filescan{scan | docsep_positions: positions}
  end

  defp gen_docsep_offsets(scan) do
    offsets = scan.docsep_positions
              |> gen_offsets([])
    %Searchex.Command.Build.Catalog.Filescan{scan | docsep_offsets: offsets}
  end

  defp gen_offsets([], list), do: list
  defp gen_offsets([head|tail], list) do
    gen_offsets(tail, list ++ [head - Enum.sum(list)])
  end
end
