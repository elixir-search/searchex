defmodule Util.Ext.File do

  @moduledoc false

  @doc """
  Recursive file list from the current directory.

  ## Example
  
      iex> is_list Searchex.FileExt.ls_r
      true

  """
  def ls_r do
    ls_r "."
  end

  @doc """
  Recursive file list from a specific path.  The path can be absolute or
  relative.

  ## Examples
  
      iex> is_list Searchex.FileExt.ls_r(".")
      true

      iex> is_list Searchex.FileExt.ls_r("./test/_fixture")
      true

  """
  def ls_r(path) do
    cond do
      File.regular?(path) -> [path]
      File.dir?(path) ->
        File.ls!(path)
        |> Enum.map(&Path.join(path, &1))
        |> Enum.map(&ls_r/1)
      true -> []
    end
    |> List.flatten
  end

  @doc """
  Recursive file list from a set of paths.  Paths can be absolute or relative.
  """
  def ls_r(paths, globs) when is_list(paths) do
    paths
    |> Enum.map(&ls_r(&1, globs))
    |> List.flatten
  end

  @doc """
  Recursive file list from a specific path.  The path can be absolute or
  relative.  Second parameter takes filename extensions for filtering - can be
  a list or a single string.

  ## Examples
  
      iex> is_list Searchex.FileExt.ls_r(".", ["ex", "exs"])
      true

      iex> is_list Searchex.FileExt.ls_r("./test/_fixture", ["txt", "md"])
      true

      iex> is_list Searchex.FileExt.ls_r(".", "ex")
      true

      iex> is_list Searchex.FileExt.ls_r("./test/_fixture", "txt")
      true

  """
  def ls_r(path, globs) when is_list(globs) do
    ls_r(path)
    |> glob_filter(globs)
  end

  def ls_r(path, glob) do
    ls_r(path)
    |> Enum.filter(&glob_match?(&1, glob))
  end

  @doc """
  Get size of directories
  """
  def du_s(path_list) when is_list(path_list) do
    path_list
    |> Enum.map(fn(path) -> du_s(path) end)
    |> Enum.sum
  end
  def du_s(path) do
    path
    |> ls_r
    |> Enum.map(fn(path) -> get_size(path) end)
    |> Enum.sum
  end

  defp get_size(path) do
    if File.exists?(path) do
      %{size: size} = File.stat!(path)
      size
    else
      0
    end
  end

  # --------------------------------------------------------------------

  # compiles a string (like "txt") into a regex.
  # the regex matches EOL (eg> ~r/txt!/)
  defp reg_compile(glob) do
    {:ok, reg} = Regex.compile("#{glob}$")
    reg
  end

  # eg> glob_match?("/my/path", ["txt", "md"]) 
  defp glob_match?(path, globs) when is_list(globs) do
    globs
    |> Enum.any?(&glob_match?(path, &1))
  end

  # eg> glob_match?("/my/path", "txt") 
  defp glob_match?(path, glob) do
    String.match?(path, reg_compile(glob))
  end

  # eg> glob_filter(["/path1", "path2"], ["txt", "md"])
  defp glob_filter(paths, globs) do
    paths 
    |> Enum.filter(&glob_match?(&1, globs))
  end
end
