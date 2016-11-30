defmodule Searchex.Command.Build.Catalog.Cache do

  @moduledoc false

  @compile_time System.cmd("date", ["+%Y %m %d %H %M %S"])

  @doc "Write catalog to output file"
  def write_catalog(scan) do
    path = cache_file(scan)
    string = :erlang.term_to_binary(scan)
    File.write(path, string)
    scan
  end

  @doc "Read catalog from input file"
  def read_catalog(params) do
    path = cache_file(params)
    {:ok, string} = File.read(path)
    :erlang.binary_to_term(string)
  end

  def stale?(params) do
    path = cache_file(params)
    if File.exists?(path) do
      {time, _} = @compile_time
      ctime = time |> String.split |> Enum.map(&String.to_integer/1)
      latest = Enum.max([ctime, max_file_time(Map.get(params, :doc_dirs))])
      path_time(path) < latest
    else
      true
    end
  end

  # ----------------------------------------------------

  defp cache_file(scan) do
    collection = Map.get(scan, :collection) || scan.params.collection
    base_dir   = Path.expand("~/.searchex/data")
    File.mkdir_p!(base_dir)
    base_dir <> "/#{collection}_cat.dat"
  end

  defp max_file_time(array) do
    array
    |> Enum.map(fn(path) -> Path.expand(path) |> Searchex.Util.File.ls_r end)
    |> List.flatten
    |> Enum.map(fn(path) -> path_time(path) end)
    |> Enum.max
  end

  defp path_time(path) do
    epath = Path.expand(path)
    {:ok, info} = File.stat(epath, time: :local)
    {{a,b,c},{d,e,f}} = Map.get(info, :mtime)
    [a,b,c,d,e,f]
  end
end
