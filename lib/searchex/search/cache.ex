defmodule Searchex.Search.Cache do

  @moduledoc false
  
  @doc "Write results to cache file"
  def write_results({catalog, results}) do
    DIO.inspect :SAVE
    path   = cache_file
    string = :erlang.term_to_binary(results)
    File.write(path, string)
    {catalog, results}
  end

  @doc "Read index from input file"
  def read_results do
    path = cache_file
    {:ok, string} = File.read(path)
    :erlang.binary_to_term(string)
  end

#  def stale?(params) do
#    idx_path  = cache_file(params, "idx")
#    cat_path  = cache_file(params, "cat")
#    {time, _} = "TBD"
#    ctime     = time |> String.split |> Enum.map(&String.to_integer/1)
#    if File.exists?(idx_path) do
#      path_time(idx_path) < Enum.max([path_time(cat_path), ctime])
#    else
#      true
#    end
#  end

  # ----------------------------------------------------

  defp cache_file do
    base_dir   = Path.expand("~/.searchex/temp")
    File.mkdir_p!(base_dir)
    base_dir <> "/results.dat"
  end

  defp path_time(path) do
    epath = Path.expand(path)
    {:ok, info} = File.stat(epath, time: :local)
    {{a,b,c},{d,e,f}} = Map.get(info, :mtime)
    [a,b,c,d,e,f]
  end
end
