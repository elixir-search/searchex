defmodule Util.Ext.File do

  @moduledoc false

  @doc """
  Recursive file list from the current directory.
  """
  def ls_r do
    ls_r "."
  end

  @doc """
  Recursive file list from a specific path.  The path can be absolute or
  relative.
  """
  def ls_r(paths), do: ls_r(paths, %{})

  def ls_r(paths, opts) when is_list(paths) do
    lcl_opts = default_opts(opts)
    paths
    |> Enum.map(fn(path) -> ls_process(path, lcl_opts) end)
    |> List.flatten
    |> Enum.take(lcl_opts.maxnum)
  end

  def ls_r(path, opts), do: ls_r([path], opts)

  # --------------------------------------------------------------------

  defp default_opts(opts) do
    defaults = %{
      types:    []    ,
      skips:    []    ,
      maxnum:   1000  ,
      depth:    10    ,
    }
    Map.merge(defaults, opts)
  end

  defp ls_process(path, opts) do
    if File.dir?(path), do: ls_dir(path, opts), else: ls_file(path, opts)
  end

  # --------------------------------------------------------------------

  defp ls_dir(_path, %{depth: 0} = _opts), do: []
  defp ls_dir(base_path, opts) do
    new_opts = %{opts | depth: opts.depth - 1}
    result = dirglob_match?(base_path, opts.skips)
    if result do
      []
    else
      base_path
      |> File.ls!
      |> Enum.map(fn(path) -> base_path <> "/" <> path end)
      |> Enum.map(fn(path) -> ls_r(path, new_opts) end)
    end
  end

  defp ls_file(path, opts) do
    if fileglob_match?(path, opts.types), do: [path], else: []
  end

  # --------------------------------------------------------------------

  defp fileglob_match?(path, globs) do
    newglobs = Enum.map(globs, fn(glob) -> "\\.#{glob}$" end)
    case newglobs do
      []       -> true
      newglobs -> Enum.any?(newglobs, &glob_match?(path, &1))
    end
  end

  defp dirglob_match?(path, globs) do
    dirpath = String.split(path, "/") |> List.last
    case globs do
      []    -> false
      globs -> Enum.any?(globs, &glob_match?(dirpath, &1))
    end
  end

  # eg> glob_match?("/my/path", "txt")
  defp glob_match?(path, glob) do
    String.match?(path, reg_compile(glob))
  end

  # compiles a string (like "txt") into a regex.
  # the regex matches EOL (eg> ~r/txt!/)
  defp reg_compile(glob) do
    {:ok, reg} = Regex.compile(glob)
    reg
  end

  # --------------------------------------------------------------------

  @doc """
  Get size of directories
  """
  def du_s(path_list, opts) when is_list(path_list) do
    path_list
    |> ls_r(opts)
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
end
