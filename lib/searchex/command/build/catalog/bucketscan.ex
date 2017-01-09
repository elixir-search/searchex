defmodule Searchex.Command.Build.Catalog.Bucketscan do
  @moduledoc false

  defstruct rawdata:          ""    ,
            bucket_id:        ""    ,
            docsep_positions: []    ,
            docsep_locations: []

  alias Searchex.Command.Build.Catalog.Bucketscan

  def generate_bucketscan(filename, frame) do
    %Bucketscan{bucket_id: filename}
    |> read_rawdata(frame)
    |> gen_docsep_positions(frame.params)
    |> gen_docsep_locations
  end

  defp read_rawdata(bucketscan, frame) do
    adapter = Searchex.Adapter.adapter_module(frame)
    rawdata = adapter.rawdata(bucketscan.bucket_id, frame)
    %Bucketscan{bucketscan | rawdata: rawdata}
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
    %Bucketscan{scan | docsep_positions: [0] ++ positions}
  end

  defp gen_docsep_locations(scan) do
    dlen = Util.Ext.String.len(scan.rawdata)
    pos1 = scan.docsep_positions
    [_h|tail] = pos1
    pos2 = tail ++ [dlen]
    locs = Enum.zip(pos1, pos2)
           |> Enum.map(fn({beg, fin}) -> {beg, fin - beg} end)
           |> Enum.filter(fn({_beg, len}) -> len > 0 end)
    %Bucketscan{scan | docsep_locations: locs}
  end
end
