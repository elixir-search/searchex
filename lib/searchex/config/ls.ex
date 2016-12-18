defmodule Searchex.Config.Ls do
  @moduledoc false

  def exec do
    File.mkdir_p Searchex.base_dir
    list = repo_dirs
           |> repo_files
           |> cfg_files
           |> file_parts
           |> uniq_names
    {:ok, list}
  end

  defp repo_dirs do
    Searchex.base_dir
    |> File.ls
    |> elem(1)
    |> Enum.map(fn(path) -> Path.expand("#{Searchex.base_dir}" <> "/" <> path) end)
    |> Enum.filter(fn(path) -> File.dir?(path) end)
  end

  defp repo_files(repo_dirs) do
    repo_dirs
    |> Enum.map(fn(dir) -> flist(dir) end)
    |> List.flatten
  end

  defp cfg_files(repo_files) do
    repo_files
    |> Enum.filter(fn(pp) -> Regex.match?(~r/yml$/, pp) end)
  end

  defp file_parts(cfg_files) do
    cfg_files
    |> Enum.map(fn(file) -> opt_fn(file) end)
    |> Enum.sort
  end

  defp uniq_names(cfg_files) do
    cfg_files
  end

  defp flist(dir) do
    File.ls(dir)
    |> elem(1)
    |> Enum.map(fn(file) -> dir <> "/" <> file end)
  end

  defp opt_fn(file) do
    file
    |> String.split("/")
    |> Enum.take(-2)
    |> Enum.join("/")
    |> String.replace(~r/.yml$/, "")
  end
end