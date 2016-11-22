defmodule Searchex.Build.Index.Cache do

  @moduledoc false

  @compile_time System.cmd("date", ["+%Y %m %d %H %M %S"])

  alias Searchex.Util.IO, as: DO

  @doc "Write index to output file"
  def write_index(scan) do
    path = cache_file(scan)
    string = :erlang.term_to_binary(otp_to_map)
    File.write(path, string)
    :ok
  end

  @doc "Read index from input file"
  def read_index(params) do
    path = cache_file(params)
    {:ok, string} = File.read(path)
    term = :erlang.binary_to_term(string)
    map_to_otp term
  end

  def stale?(params) do
    idx_path  = cache_file(params, "idx")
    cat_path  = cache_file(params, "cat")
    {time, _} = @compile_time
    ctime     = time |> String.split |> Enum.map(&String.to_integer/1)
    if File.exists?(idx_path) do
      path_time(idx_path) < Enum.max([path_time(cat_path), ctime])
    else
      true
    end
  end

  # ----------------------------------------------------

  defp otp_to_map do
    list = Supervisor.which_children(Searchex.KeywordSup)
    Enum.reduce list, %{}, fn({child, _, _, _}, acc) ->
      vals = GenServer.call(child, :get_ids)
      Map.merge acc, %{child => vals}
    end
  end

  defp map_to_otp(map) do
    remove_all_otp_children
    Map.keys(map)
    |> Enum.each(fn(key) ->
          srv = Searchex.KeywordSer.get_keyword_server(key)
          Searchex.KeywordSer.set_state srv, map[key]
       end)
  end

  defp remove_all_otp_children do
    list = Supervisor.which_children(Searchex.KeywordSup)
    Enum.each list, fn({child, _, _, _}) ->
      Supervisor.delete_child(Searchex.KeywordSup, child)
    end
  end

  defp cache_file(scan, ext \\ "idx") do
    collection = Map.get(scan, :collection) || scan.params.collection
    base_dir   = Path.expand("~/.searchex/data")
    File.mkdir_p!(base_dir)
    base_dir <> "/#{collection}_#{ext}.dat"
  end

  defp path_time(path) do
    epath = Path.expand(path)
    {:ok, info} = File.stat(epath, time: :local)
    {{a,b,c},{d,e,f}} = Map.get(info, :mtime)
    [a,b,c,d,e,f]
  end
end
