defmodule Searchex.Command.Search.Cache do

#  @moduledoc false
#
#  @doc "Write results to cache file"
#  def write_results(elem) do
#    Util.Ext.IO.inspect :WRITE_RESULTS, color: "green"
#    path   = cache_file
#    string = :erlang.term_to_binary(elem)
#    File.write(path, string)
#    elem
#  end
#
#  @doc "Read index from input file"
#  def read_results do
#    path = cache_file
#    Util.Ext.IO.inspect :READ_RESULTS, color: "green"
#    case File.read(path) do
#      {:ok, string} -> :erlang.binary_to_term(string)
#      {:error, _}   -> {:error, "ERROR #{path}"}
#     end
#  end
#
#  # ----------------------------------------------------
#
#  defp cache_file do
#    base_dir = Searchex.base_dir
#    File.mkdir_p!(base_dir)
#    base_dir <> "/results.dat"
#  end
end
