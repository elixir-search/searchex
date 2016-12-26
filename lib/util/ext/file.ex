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
  def ls_r(paths), do: ls_r(paths, [])

  def ls_r(_, [path_depth: 0, globs: _]), do: []

  def ls_r(paths, opts) when is_list(paths) do
    lcl_opts = Keyword.merge([path_depth: 4, globs: []], opts)
    Enum.map(paths, fn(path) -> ls_r(path, lcl_opts) end)
    |> List.flatten
  end

  def ls_r(path, opts) do
    lcl_opts = Keyword.merge([path_depth: 4, globs: []], opts)
    new_depth = lcl_opts[:path_depth] - 1
    new_opts = Keyword.merge(lcl_opts, [path_depth: new_depth])
    cond do
      new_depth == 0      -> [path]
      File.regular?(path) -> [path]
      File.dir?(path)     ->
        File.ls!(path)
        |> Enum.filter(fn(file) -> ! Regex.match?(~r/^[._]/, file) end)
        |> Enum.filter(fn(file) -> glob_match?(file, new_opts[:globs]) end)
        |> Enum.map(&Path.join(path, &1))
        |> Enum.map(&(ls_r(&1, new_opts)))
      true -> []
    end
    |> List.flatten
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
    {:ok, reg} = Regex.compile("\\.#{glob}$")
    reg
  end

  # eg> glob_match?("/my/path", ["txt", "md"])
  defp glob_match?(path, globs) when is_list(globs) do
    case globs do
      []    -> true
      globs -> Enum.any?(globs, &glob_match?(path, &1))
    end
  end

  # eg> glob_match?("/my/path", "txt") 
  defp glob_match?(path, glob) do
    String.match?(path, reg_compile(glob))
  end
end
