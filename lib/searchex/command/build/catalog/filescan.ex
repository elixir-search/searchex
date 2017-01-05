defmodule Searchex.Command.Build.Catalog.Filescan do
  @moduledoc false

  defstruct rawdata:          ""    ,
            input_filename:   ""    ,
            docsep_positions: []    ,
            docsep_locations: []

  alias Searchex.Command.Build.Catalog.Filescan

  def generate_filescan(filename, params) do
    %Filescan{input_filename: filename}
    |> read_rawdata(params.file_maxkb)
    |> gen_docsep_positions(params)
    |> gen_docsep_locations
  end
  
  defp read_rawdata(scan, file_maxkb) do
    rawdata = File.stream!(scan.input_filename, [], file_maxkb * 1024) |> Enum.at(0)
    %Filescan{scan | rawdata: rawdata}
  end

  defp gen_docsep_positions(scan, params) do
    positions = if params.docsep do
                  params.docsep
                  |> Regex.compile |> elem(1)
                  |> Regex.scan(scan.rawdata, return: :index )
                  |> Enum.map(fn(x) -> [{beg, fin} | _tail] = x; beg + fin end)
                else
                  [0]
                end
    %Filescan{scan | docsep_positions: [0] ++ positions}
  end

  defp gen_docsep_locations(scan) do
    dlen = Util.Ext.String.len(scan.rawdata)
    pos1 = scan.docsep_positions
    [_h|tail] = pos1
    pos2 = tail ++ [dlen]
    locs = Enum.zip(pos1, pos2)
           |> Enum.map(fn({beg, fin}) -> {beg, fin - beg} end)
           |> Enum.filter(fn({_beg, len}) -> len > 0 end)
    %Filescan{scan | docsep_locations: locs}
  end
end
