defmodule Searchex.Search.Cache do

  @moduledoc false
  
  @doc "Write results to cache file"
  def write_results({catalog, results}) do
    DIO.inspect :SAVE
    path   = cache_file
    string = :erlang.term_to_binary({catalog, results})
    File.write(path, string)
    {catalog, results}
  end

  @doc "Read index from input file"
  def read_results do
    path = cache_file
    {:ok, string} = File.read(path)
    :erlang.binary_to_term(string)
  end

  # ----------------------------------------------------

  defp cache_file do
    base_dir   = Path.expand("~/.searchex/temp")
    File.mkdir_p!(base_dir)
    base_dir <> "/results.dat"
  end
end
