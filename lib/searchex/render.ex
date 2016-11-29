defmodule Searchex.Render do
  @moduledoc """
  CLI rendering for anything more sophisticated than `IO.puts`

  - editor launching
  - table rendering
  """

  @doc """
  Invoke `Command.cfg_edit`, then launches an editor to open the pathname.
  """
  def cfg_edit(cfg_name) do
    {:ok, path, linenum} = Searchex.Config.cfg_edit(cfg_name)
    EditorLaunch.exec(path, line_num: linenum)
    :ok
  end

  @doc """
  Invoke `Command.search`, then render the results as a table.
  """
  def search(_cfg_name, _query) do
#    results = Searchex.Command.search(cfg_name, query)
#    IO.puts results
    # RENDER AS TABLE
    :ok
  end

  @doc """
  Alias for search...
  """
  def query(_cfg_name, _query) do
#    search(cfg_name, query)
    :ok
  end

  @doc """
  Render the search results as a table.
  """
  def results do
#    results = Searchex.Command.results
#    IO.puts results
    :ok
  end

  # -----

#  defp render_table(args) do
#    "TBD"
#  end

#  defp editor_launch(file_path, args \\ []) do
#    EditorLaunch.exec(file_path, args)
#  end
end